structure tigerflow :> tigerflow = 
struct

type flowgraph = {control: tigergraph.graph, (* Grafo dirigido donde cada nodo representa una instruccion *)
						     def: (tigertemp.temp,tigertemp.temp list) tigertab.Tabla, (* Tabla de temporarios definidos en cada nodo *)
						     use: (tigertemp.temp,tigertemp.temp list) tigertab.Tabla, (* Tabla de temporarios usados en cada nodo *)
						     ismove: (tigertemp.temp,bool) tigertab.Tabla} (* Indica si cada instruccion es una instruccion move, que podria eliminarse si def y use son iguales *)

fun instr2graph instr = ({control=tigergraph.newGraph (),def=tigertab.tabNueva (),use=tigertab.tabNueva (),ismove=tigertab.tabNueva ()},[])

end