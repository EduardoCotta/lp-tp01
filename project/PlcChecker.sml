(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun hasEqualImplemented t =
    case t of 
        (IntT) => true |
        (BoolT) => true |
        (ListT []) => true |
        (SeqT x) => hasEqualImplemented x |
        _ => false

fun indexInbouds i list = (i > 0 andalso i <= List.length(list))

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
		  ConI _ => IntT
		| ConB _ => BoolT
		| Var x => lookup env x
		| ESeq(SeqT(t)) => SeqT t
		| Anon(t, x, e1) => FunT(t, teval e1 ((x, t)::env))
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr, t1) of
						 ("print", _) => ListT [] 
                         | ("!", t1) => if t1 = BoolT then BoolT else raise UnknownType 
                         | ("hd", t1) => (case t1 of 
                                         (SeqT, seqType) => seqType |
                                         _ => raise OpNonList) 
                         | ("tl", t1) => (case t1 of 
                                         (SeqT, seqType) => t1 |
                                         _ => raise OpNonList) 
                         | ("-", t1) => if t1 = IntT then IntT else raise UnknownType 
                         | ("ise", t1) => (case t1 of 
                                          (SeqT, _) => BoolT |
                                           _ => raise UnknownType) |  
						 | _ => raise UnknownType
				end
		| Prim2(opr, e1, e2) =>
				let
					val t1 = teval e1 env
					val t2 = teval e2 env
				in
					case (opr, t1, t2) of
					  ("*" , IntT, IntT) => IntT
					| ("/" , IntT, IntT) => IntT
					| ("+" , IntT, IntT) => IntT
					| ("-" , IntT, IntT) => IntT
					| (";" , _ , _)    => t2
					| ("::" , t1 , t2) => if SeqT(t1) = t2 then SeqT t1 else raise NotEqTypes
					| ("<" , t1 , t2) => if t1 = IntT andalso t2 = IntT then BoolT else raise UnknownType
					| ("<=" , t1 , t2) => if t1 = IntT andalso t2 = IntT then BoolT else raise UnknownType
					| ("=" , t1 , t2) => if t1 = t2 andalso hasEqualImplemented t1 then BoolT else raise NotEqTypes
					| ("!=" , t1 , t2) => if t1 = t2 andalso hasEqualImplemented t1 then BoolT else raise NotEqTypes
					| ("&&" , t1 , t2) => if t1 = BoolT andalso t2 = BoolT then BoolT else raise NotEqTypes
					| _   =>  raise UnknownType
				end
		| Let(x, e1, e2) =>
				let
					val t = teval e1 env
					val env' = (x,t)::env
				in
					teval e2 env'
				end
		| Letrec(fName, argType, argName, returnType, e1, e2) => 
			(let
				val t1 = teval e1 ((fName, FunT(argType, returnType ))::(argName, argType)::env);
				val t2 = teval e2 ((fName, FunT(argType, returnType ))::env);
			 in (
				if t1 = returnType then t2 else raise WrongRetType
			 )
			 end
			)
		| Match(_, []) => raise NoMatchResults
		| If(e1, e2, e3) => 
			if (teval e1 env = BoolT) then 
				(let
					val t2 = teval e2 env;
					val t3 = teval e3 env;
				in (
					if t2 = t3 then t2 else raise DiffBrTypes
				)
				end
				)
			else
				raise IfCondNotBool
		| Item(i,  e1) => 
            (let
                val t1 = teval e1 env;
            in
                case t1 of 
                    ListT list => if indexInbouds i list 
								  then List.nth(list, i - 1) 
								  else raise ListOutOfRange
                    | _ => raise OpNonList
            end)
		| Call(e1, e2) => 
			(let 
				val t1 = teval e1 env;
				val t2 = teval e2 env;
			 in
			 (
				case t1 of
					(FunT(argType, returnType)) => 
						if t2 = argType 
						then returnType 
						else raise CallTypeMisM
					| _ => raise NotFunc
			 )
			 end)
		| List([]) => ListT([])
		| List(x::xs) => (
			let 
				val tHead = teval x env;
				val tailListTypes = (let 
										val tTail = teval List(xs) env;
									in (
										case tTail of
											ListT t => t
											| _ => raise OpNonList
									)
									end)
			in (
				ListT(tHead::tailListTypes)
			)
			end
		)
		| _   =>  raise UnknownType
