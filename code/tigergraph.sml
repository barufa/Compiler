structure tigergraph :> tigergraph = 
struct

type node = int

type graph = {maxNumNodes: int,
							pred: (node ,node Splayset.set) Splaymap.dict,
							succ: (node ,node Splayset.set) Splaymap.dict}

fun nodes graph = List.map (#1 o Splaymap.listItems) (#succ graph)

fun succ graph node = case peek(#succ graph,node) of
												SOME value => Splayset.listItems value
												| _ => raise notExistNode

fun pred graph node = case peek(#pred graph,node) of
												SOME value => Splayset.listItems value
												| _ => raise notExistNode

fun adj graph node = if isNode node then Splayset.union(Splaymap.find(#pred graph,node),Splaymap.find(#succ graph,node))
										 else raise notExistNode
										
(* Chequear *)

fun eq (node1,node2) = node1 = node2

fun newGraph () = {maxNumNodes = 0,
									 pred = mkDict Int.compare,
									 succ = mkDict Int.compare}

fun newNode graph = {maxNumNodes = (#maxNumNodes graph) + 1,
										 pred = Splaymap.insert(#pred graph,(#maxNumNodes graph) + 1,Splayset.empty Int.compare),
										 succ = Splaymap.insert(#succ graph,(#maxNumNodes graph) + 1,Splayset.empty Int.compare)}

fun isNode graph node =
	let a = node > 0 andalso node <= #maxNumNodes graph (* Compruebo que este entre 0 y maxNumNodes del grafo *)
			b = case Splaymap.peek(#pred graph,node) of (* Compruebo que exista en el diccionario de predecesores *)
						SOME _ => true
						| NONE => false
			c = case Splaymap.peek(#succ graph,node) of (* Compruebo que exista en el diccionario de sucesores *)
						SOME _ => true
						| NONE => false
	in if a andalso b andalso c then true else false
	end

fun isEdge graph {from, to} = case Splaymap.peek(#succ graph, from) of
																SOME value => Splayset.member(value,to)
																| NONE => raise notExistNode

fun isNotEdge graph {from, to} = case Splaymap.peek(#succ graph, from) of
																	SOME value => case Splayset.member(value,to) of
																									true => false
																									false => true
																	| NONE => raise notExistNode

fun mk_edge graph {from, to} = if isEdge graph {from, to}
															 then graph
															 else {maxNumNodes = #maxNumNodes graph,
													 					pred = Splaymap.insert(#pred graph,to,Splayset.add(Splaymap.find(#pred graph,to),from)),
													 					succ = Splaymap.insert(#succ graph,from,Splayset.add(Splaymap.find(#succ graph,from),to))}

fun rm_edge graph {from, to} = if isNotEdge graph {from, to}
															 then graph
															 else {maxNumNodes = #maxNumNodes graph,
																		 pred = Splaymap.insert(#pred graph,to,Splayset.delete(Splaymap.find(#pred graph,to),from)),
																		 succ = Splaymap.insert(#succ graph,from,Splayset.delete(Splaymap.find(#succ graph,from),to))}

fun nodename node = Int.toString node

end