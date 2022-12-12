(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr) (env:plcVal env) : plcVal =
	case e of
		  ConI i => IntV i
		| ConB i => BoolV i
		| List [] => ListV []
		| List (x::xs) => let
			val e1 = eval x env;
			val e2 = let
				val v = eval(List xs) env;
			in
				case v of
					ListV a => a
					| SeqV a => a
					| _ => raise Impossible
			end
		in
			ListV(e1::e2)
		end
		| Item(e1, e2) => let
				val i = eval e2 env;
				val newList = let
					val v = i;
				in
					case v of
						ListV x => x
						| SeqV x => x
						| _ => raise Impossible
				end
			in
				List.nth(newList, e1-1)
			end
		| ESeq _ => SeqV []
		| Var x => lookup env x
		| If(e1, e2, e3) => 
			if eval e1 env = (BoolV true) then eval e2 env else eval e3 env
		| Anon (e1, e2, e3) => Clos("", e2, e3, env)
		| Prim1(opr, e1) =>
				let
					val v1 = eval e1 env
				in
					case (opr, v1) of
						  ("-", IntV i) => IntV (~i)
						| ("print", _) =>
										let
											val s = val2string v1
										in
											print(s^"\n"); ListV []
										end
						| ("!", BoolV i) => BoolV(not i)
						| ("hd", l1) => 
							(case l1 of
								SeqV(x::xs: plcVal list) => x
								| SeqV [] => raise HDEmptySeq
								| _ => raise Impossible)
						| ("tl", l1) => 
							(case l1 of
								SeqV(x::xs: plcVal list) => SeqV(xs)
								| SeqV [] => raise TLEmptySeq
								| _ => raise Impossible)
						| ("ise", l1) => 
							(case l1 of
								SeqV [] => BoolV true
								| _ => BoolV false)

						| _   => raise Impossible
						end
		| Prim2(opr, e1, e2) =>
				let
					val v1 = eval e1 env
					val v2 = eval e2 env
				in
					case (opr, v1, v2) of
						  ("*" , IntV i1, IntV i2) => IntV (i1 * i2)
						| ("/" , IntV i1, IntV i2) => IntV (i1 div i2)
						| ("+" , IntV i1, IntV i2) => IntV (i1 + i2)
						| ("-" , IntV i1, IntV i2) => IntV (i1 - i2)
						| ("=", IntV i1, IntV i2) => BoolV (i1 = i2)
						| ("<", IntV i1, IntV i2) => BoolV (i1 < i2)
						| ("<=", IntV i1, IntV i2) => BoolV (i1 <= i2)
						| ("!=", IntV i1, IntV i2) => BoolV (not(i1 = i2))
						| (";" , _ , _) => v2
						| _ => raise Impossible
						end
		| Let(x, e1, e2) =>
				let
					val v = eval e1 env
					val env2 = (x,v) :: env
				in
					eval e2 env2
				end
		| Letrec(nameFun, _, nameVar, _, e1, e2) => let
			val clo = Clos(nameFun, nameVar, e1, env);
			val newEnv = (nameFun, clo)::env;
		in
			eval e2 newEnv
		end
		| Call(Var v, e) => let
			val funVar = lookup env v
		in
			case funVar of
				Clos(v, x, e1, funState) => let
					val valE = eval e env;
					val newEnv = ((x, valE)::(v, funVar)::funState);
				in
					eval e1 newEnv
				end
				| _ => raise NotAFunc
		end
		| Call(Call f, e) => (let
			val funVar = eval (Call f) env;
		in
			case funVar of
				Clos(f, x, e1, funState) => let
					val valE = eval e env;
					val newEnv = (x, valE)::(f, funVar)::funState;
				in
					eval e1 newEnv
				end
				| _ => raise NotAFunc
		end)
		| _ => raise Impossible