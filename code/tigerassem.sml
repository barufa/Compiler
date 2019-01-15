structure tigerassem = struct

type reg = string
type temp = tigertemp.temp
type label = tigertemp.label

datatype instr = IOPER of {assem: string,
											    dst: temp list,
											    src: temp list,
											    jump: label list option}
               | ILABEL of {assem: string, lab: label}
               | IMOVE of {assem: string,
											    dst: temp,
											    src: temp}

(*format : (temp->string) -> instr -> string
 formats an assembly instruction as a string; with a function that
 shows the register assignment of every temp
 Pagina 201
*)
fun format mapRegister (IOPER{assem,dst,src,jump}) = assem^" \n"
  | format mapRegister (ILABEL{assem,...}) = assem^": "
  | format mapRegister (IMOVE{assem,dst,src}) = assem^" \n"
  | format _           _ = raise Fail "format: Caso no contemplado"

fun formatCode (IOPER{assem,dst,src,jump}) = "OPER: "^assem
  | formatCode (ILABEL{assem,...})         = "LABEL: "^assem^": "
  | formatCode (IMOVE{assem,dst,src})      = "MOVE: "^assem
  | formatCode _                           = raise Fail "format: Caso no contemplado"

end
