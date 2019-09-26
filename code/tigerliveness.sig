signature tigerliveness =
sig

datatype igraph =
	IGRAPH of {graph: tigergraph.graph,													(* Grafo de interferencia *)
						 tnode: tigertemp.temp -> tigergraph.node,				(* Mapea temporarios del programa assembler a nodos *)
						 gtemp: tigergraph.node -> tigertemp.temp,				(* Mapeo inverso al anterior, de nodos a temporarios *)
						 moves: (tigergraph.node * tigergraph.node) list}	(* Lista de instrucciones moves. Indica que seria bueno asignar a ambos nodos el mismo temporarios si fuera posible *)

val interferenceGraph: tigerflow.flowgraph -> igraph * (tigetflow. -> tigettemp.temp list)

val shcw: outstream * igraph -> unit

end
