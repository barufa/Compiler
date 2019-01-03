structure tigercodegen :> tigercodegen =
struct

open tigerframe
open tigerassem
open tigertree
open tigertemp

fun codegen frame stm = 
let val ilist = ref ([]:instr list)
		fun emit x = ilist := x::!ilist
		fun result gen = let val t = tigertemp.newtemp() in (gen t; t) end
		fun munchStm (_) = (*COMPLETAR*)

		fun muchExp (_) = (*COMPLETAR*)

		fun muchArgs (_) = (*COMPLETAR*)

in munchStm stm; rev(!ilist) end
