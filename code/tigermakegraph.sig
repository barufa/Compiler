signature tigermakegraph =
sig

	val instr2graph: tigerassem.instr list -> tigerflow.flowgraph * tigerflow.graph.node list (* Toma una lista de instrucciones y devuelve un grafo de flujo *)

end
