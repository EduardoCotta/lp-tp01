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
      LKEY | RKEY | NEGATION |
      EOF

%nonterm Prog of expr |
         Decl of expr |
         Expr of expr |
         AtmExp of expr |
         Const of expr  |
         Comps of expr list |
         Type of plcType |
         AtmType of plcType | 
         MatchExpr of (expr option * expr) list | 
         CondExpr of (expr option) |
         Types of plcType list

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

Decl: VAR NAME EQ Expr SEMICOLON Prog (Let(NAME, Expr, Prog))

Expr: AtmExp (AtmExp) |
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

AtmExp: Const (Const) |
        LPAR Comps RPAR (List(Comps)) |
        NAME (Var(NAME)) |
        LKEY Prog RKEY (Prog) |
        LPAR Expr RPAR (Expr)

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
