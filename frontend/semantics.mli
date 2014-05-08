type obj = {pos:Utils.pos}
type operator = {pos:Utils.pos; str:string}
exception Type_Error of obj * operator * obj
val analyze : Cst.cst -> Ast.ast * Symbol_table.symboltable
