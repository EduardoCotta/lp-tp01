(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun getKeywordOrName (s, lpos, rpos) = 
    case s of 
    "true" => TRUE(lpos, rpos) | 
    "false" => FALSE(lpos, rpos) |
    "Bool"  => BOOL(lpos, rpos) |
    "Int"   => INT(lpos,rpos) | 
    "Nil"   => NIL(lpos,rpos) |
    "print" => PRINT(lpos, rpos) |
    "if" => IF(lpos,rpos) |
    "else" => ELSE(lpos, rpos) |
    "not" => NOT(lpos, rpos) |
    "hd" => HD(lpos, rpos) |
    "tl" => TL(lpos, rpos) |
    "ise" => ISE(lpos, rpos) |
    "var" => VAR(lpos, rpos) |
    "match" => MATCH(lpos, rpos) |
    "with" => WITH(lpos, rpos) |
    "end" => END(lpos, rpos) |
    "then" => THEN(lpos, rpos) |
    "fun" => FUN(lpos, rpos) |
    "rec" => REC(lpos, rpos) |
    "fn" => FN(lpos, rpos) |
    "_" => UNDERSCORE(lpos, rpos) |
    _ => NAME(s, lpos, rpos)

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
whitespace=[\ \t];
digit=[0-9];
keyword=[a-zA-Z_][a-zA-Z_0-9]*;
%%
\n => (lineNumber := !lineNumber +1; lex());
{digit}+ => (NUM(valOf(Int.fromString yytext), !lineNumber, !lineNumber));
{whitespace}+ => (lex());
{keyword} => (getKeywordOrName(yytext,yypos,yypos));
"(" => (LPAR(yypos,yypos));
")" => (RPAR(yypos,yypos));
"," => (COMMA(yypos,yypos));
"[" => (LSBRACK(yypos,yypos));
"]" => (RSBRACK(yypos,yypos));
";" => (SEMICOLON(yypos, yypos));
"->" => (ARROW(yypos, yypos));
"&&" => (AND(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (NOTEQ(yypos, yypos));
"<" => (LT(yypos, yypos));
"<=" => (LTE(yypos, yypos));
"::" => (DOUBLECOLON(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (TIMES(yypos, yypos));
"/" => (DIV(yypos, yypos));
"|" => (PIPE(yypos, yypos));
"_" => (UNDERSCORE(yypos, yypos));
"!" => (NEGATION(yypos, yypos));
"{" => (LKEY(yypos, yypos));
"}" => (RKEY(yypos, yypos));
":" => (COLON(yypos, yypos));
"=>" => (FUNARROW(yypos, yypos));