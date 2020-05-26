signature tigerflow =
sig

(* structure tigergraph *) (* Estaba en el libro pero no le encontre utilidad *)

type flowgraph = {control: tigergraph.graph,                               (* Grafo dirigido donde cada nodo representa una instruccion *)
						     def: (tigergraph.node,tigertemp.temp list) Splaymap.dict, (* Tabla de temporarios definidos en cada nodo *)
						     use: (tigergraph.node,tigertemp.temp list) Splaymap.dict, (* Tabla de temporarios usados en cada nodo *)
						     ismove: (tigergraph.node,bool) Splaymap.dict}             (* Indica si cada instruccion es una instruccion move, que podria eliminarse si def y use son iguales *)

val instr2graph: tigerassem.instr list -> flowgraph * tigergraph.node list (* Toma una lista de instrucciones y devuelve un grafo de flujo *)

end
