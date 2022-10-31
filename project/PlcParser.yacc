%%

%name PlcParser

%pos int

%term NUM of int |
      TRUE | FALSE |
      LPAR | RPAR | COMMA |
      LSBRACK | RSBRACK |
      BOOL | INT | NIL |
      SEMICOLON | ARROW |
      IF | ELSE | AND | EQ | NOTEQ |
      LT | LTE | DOUBLECOLON | 
      PLUS | MINUS | TIMES | DIV |
      NOT | HD | TL | ISE | UNDERSCORE |
      PRINT | NAME of string | VAR |
      PIPE | MATCH | WITH | END | THEN |
      LKEY | RKEY | NEGATION | FUN | REC |
      COLON | FN | FUNARROW |
      EOF

%nonterm Prog of expr |
         Decl of expr |
         Expr of expr |
         AtmExpr of expr |
         AppExpr of expr |
         Const of expr  |
         Comps of expr list |
         Type of plcType |
         AtmType of plcType | 
         MatchExpr of (expr option * expr) list | 
         CondExpr of (expr option) |
         Types of plcType list | 
         Args of (plcType * string) list | 
         Params of (plcType * string) list | 
         TypedVar of (plcType * string)

%right SEMICOLON ARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ NOTEQ
%left LT LTE
%right DOUBLECOLON
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT HD TL ISE PRINT NAME
%left LSBRACK

%eop EOF

%noshift EOF

%start Prog

%%

Prog: Expr (Expr) |
       Decl (Decl)

Decl: VAR NAME EQ Expr SEMICOLON Prog (Let(NAME, Expr, Prog)) | 
      FUN NAME Args EQ Expr SEMICOLON Prog (Let(NAME, makeAnon(Args, Expr), Prog)) | 
      FUN REC NAME Args COLON Type EQ Expr SEMICOLON Prog (makeFun(NAME, Args, Type, Expr, Prog))


Expr: AtmExpr (AtmExpr) |
      AppExpr (AppExpr) |
      MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr)) |
      IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3)) |
      Expr LSBRACK NUM RSBRACK (Item(NUM, Expr)) |
      NEGATION Expr (Prim1("!", Expr)) | 
      HD Expr (Prim1("hd", Expr)) | 
      TL Expr (Prim1("tl", Expr)) | 
      PRINT Expr (Prim1("print", Expr)) | 
      ISE Expr (Prim1("ise", Expr)) | 
      Expr SEMICOLON Expr (Prim2(";", Expr1 , Expr2)) |
      Expr DOUBLECOLON Expr (Prim2("::", Expr1 , Expr2)) |
      Expr PLUS Expr (Prim2("+", Expr1 , Expr2)) | 
      Expr MINUS Expr (Prim2("-", Expr1 , Expr2)) | 
      Expr TIMES Expr (Prim2("*", Expr1 , Expr2)) | 
      Expr DIV Expr (Prim2("/", Expr1 , Expr2)) | 
      Expr AND Expr (Prim2("&&", Expr1 , Expr2)) | 
      Expr EQ Expr (Prim2("=", Expr1 , Expr2)) | 
      Expr NOTEQ Expr (Prim2("!=", Expr1 , Expr2)) | 
      Expr LT Expr (Prim2("<", Expr1 , Expr2)) | 
      Expr LTE Expr (Prim2("<=", Expr1 , Expr2)) | 
      MINUS Expr (Prim1("-", Expr))

AtmExpr: Const (Const) |
        LPAR Comps RPAR (List(Comps)) |
        NAME (Var(NAME)) |
        LKEY Prog RKEY (Prog) |
        LPAR Expr RPAR (Expr) | 
        FN Args FUNARROW Expr END (makeAnon(Args, Expr))

AppExpr: AtmExpr AtmExpr (Call(AtmExpr1, AtmExpr2)) |
         AppExpr AtmExpr (Call(AppExpr, AtmExpr))

Const: NUM (ConI(NUM)) |
      TRUE (ConB(true)) |
      FALSE (ConB(false)) |
      LPAR RPAR (List([])) |
      LPAR Type LSBRACK RSBRACK RPAR (ESeq(Type))

Comps: Expr COMMA Expr (Expr1::Expr2::[]) |
       Expr COMMA Comps (Expr::Comps)

Type: AtmType (AtmType) |
      LSBRACK Type RSBRACK (SeqT(Type)) |
      LPAR Types RPAR (ListT(Types)) |
      Type ARROW Type (FunT(Type1, Type2))

AtmType: BOOL (BoolT) |
         INT (IntT) | 
         NIL (ListT([])) |
         LPAR Type RPAR (Type)

Types: Type COMMA Type (Type1::Type2::[]) |
       Type COMMA Types (Type::Types)

MatchExpr: END ([]) | 
           PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr: Expr (SOME Expr) | 
          UNDERSCORE (NONE)

Args: LPAR RPAR ([]) | 
      LPAR Params RPAR (Params)

Params: TypedVar (TypedVar::[]) | 
        TypedVar COMMA Params (TypedVar::Params)

TypedVar: Type NAME (Type, NAME)