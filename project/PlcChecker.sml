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

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
		  ConI _ => IntT
		| Var x => lookup env x
		| ESeq(SeqT(t)) => SeqT t
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
		| _   =>  raise UnknownType
