signature tigerliveness =
sig

type igraph

val interferenceGraph: tigerflow.flowgraph -> igraph * (tigergraph.node,tigertemp.temp Splayset.set) Splaymap.dict (* Devuelve el grafo de interferencia y un diccionario con cada nodo y sus temporarios que estan liveOut *)

val show: igraph -> unit

end
