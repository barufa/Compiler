signature tigerflow =
sig

(* structure tigergraph *)
datatype flowgraph =
	FGRAPH of {control: tigergraph.graph, (* Grafo dirigido donde cada nodo representa una instruccion *)
						 def: tigertemp.temp list tigergraph.Table.table, (* Tabla de temporarios definidos en cada nodo *)
						 use: tigertemp.temp list tigergraph.Table.table, (* Tabla de temporarios usados en cada nodo *)
						 ismove: bool tigergraph.Table.table} (* Indica si cada instruccion es una instruccion move, que podria eliminarse si def y use son iguales *)

val instr2graph: tigerassem.instr list -> tigerflow.flowgraph * tigerflow.graph.node list (* Toma una lista de instrucciones y devuelve un grafo de flujo *)

end
