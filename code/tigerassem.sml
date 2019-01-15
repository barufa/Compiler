structure tigerassem = struct

type reg = string
type temp = tigertemp.temp
type label = tigertemp.label

datatype instr = OPER of {assem: string,
											    dst: temp list,
											    src: temp list,
											    jump: label list option}
               | LABEL of {assem: string, lab: label}
               | MOVE of {assem: string,
											    dst: temp,
											    src: temp}

(*format : (temp->string) -> instr -> string
 formats an assembly instruction as a string; with a function that
 shows the register assignment of every temp
 Pagina 201
*)
fun format mapRegister (OPER{assem,dst,src,jump}) = assem^" \n"
  | format mapRegister (LABEL{assem,...}) = assem^": "
  | format mapRegister (MOVE{assem,dst,src}) = assem^" \n"
  | format _           _ = raise Fail "format: Caso no contemplado"

fun formatCode (OPER{assem,dst,src,jump}) = "OPER: "^assem
  | formatCode (LABEL{assem,...})         = "LABEL: "^assem^": "
  | formatCode (MOVE{assem,dst,src})      = "MOVE: "^assem
  | formatCode _                          = raise Fail "format: Caso no contemplado"

end
