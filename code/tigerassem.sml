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

(* Para que muestre los registros temporarios*)

fun stint2 (x::xs) ns =
	if Char.isDigit x then stint2 xs (ns^Char.toCString(x))
	else (valOf(Int.fromString(ns)),(x::xs))
	| stint2 [] ns = (valOf(Int.fromString(ns)),[])

fun stint ls = stint2 ls ""

fun fillreg (x::xs) src dst jmp =
		if x<>(#"'") then Char.toCString(x)^(fillreg xs src dst jmp)
		else let val (n,ys) = stint(List.tl(xs))
						 val ls = (case List.hd(xs) of
														#"s" => src
													| #"d" => dst
													|  jl  => jmp)
					in List.nth(ls,n)^(fillreg ys src dst jmp) end
  | fillreg [] _ _ _ = ""

(**********************************************)

fun format mapRegister (IOPER{assem,dst,src,jump}) = assem^" \n"
  | format mapRegister (ILABEL{assem,...}) = assem^": "
  | format mapRegister (IMOVE{assem,dst,src}) = assem^" \n"
(*
  | format _           _ = raise Fail "format: Caso no contemplado"
*)

fun formatCode (IOPER{assem,src,dst,jump}) =
			let val jmp = if jump = NONE then [] else valOf jump
					val s = fillreg (String.explode assem) src dst jmp
			in print(s^"\n") end
   | formatCode (IMOVE{assem,src,dst})      =
			let val s = fillreg (String.explode assem) [src] [dst] []
			in print(s^"\n") end
   | formatCode (ILABEL{assem,...})         =
			print(assem^"\n")
(*
  | formatCode _                           = raise Fail "format: Caso no contemplado"
*)

fun showCode (ILABEL{assem,...}) = (assem^"\n")
  | showCode (IOPER{assem,src,dst,jump}) =
    		let val jmp = if jump = NONE then [] else valOf jump
				val s = fillreg (String.explode assem) src dst jmp
			in (s^"\n") end
  | showCode (IMOVE{assem,src,dst}) =
			let val s = fillreg (String.explode assem) [src] [dst] []
			in (s^"\n") end

end
