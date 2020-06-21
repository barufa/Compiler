signature tigerregalloc =
sig

structure Frame: FRAME

type allocation = (tigertemp.temp,tigerframe.register) Splaymap.dict

val alloc: tigerassem.instr * tigerframe.frame -> tigerassem.instr list * allocation

end