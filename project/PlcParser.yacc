%%

%name PlcParser

%pos int

%term NUM of int |
      EOF

%nonterm Prog of expr |
         Decl of expr |
         Expr of expr |
         AtmExp of expr |
         Const of expr 

%eop EOF

%noshift EOF

%start Prog

%%

Prog: Expr (Expr) |
       Decl (Decl)

Expr: AtmExp (AtmExp)

AtmExp: Const (Const)

Const: NUM (ConI(NUM))