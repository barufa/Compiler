structure tigergraph :> tigergraph = 
struct

exception graphEdge
exception notExistNode
exception notExistEdge

type node = int

type graph = {maxNumNodes: int,
							pred: (node ,node Splayset.set) Splaymap.dict,
							succ: (node ,node Splayset.set) Splaymap.dict}
(*
fun nodes graph = List.map (#1 o Splaymap.listItems) (#succ graph)

fun nodes graph = List.map (#1) (Splaymap.listItems (#succ graph))
*)
fun nodes ({succ,...}:graph) = List.map (#1) (Splaymap.listItems succ)

fun succ ({succ,...}:graph) node = case Splaymap.peek(succ,node) of
                                    SOME value => Splayset.listItems value
												            | _ => raise notExistNode

fun pred ({pred,...}:graph) node = case Splaymap.peek(pred,node) of
												            SOME value => Splayset.listItems value
												            | _ => raise notExistNode

fun eq (node1,node2) = node1 = node2

fun newGraph () = {maxNumNodes = 0,
									 pred = Splaymap.mkDict Int.compare,
									 succ = Splaymap.mkDict Int.compare}

fun newNode ({maxNumNodes,pred,succ}:graph) =
  {maxNumNodes = maxNumNodes + 1,
  pred = Splaymap.insert(pred,maxNumNodes + 1,Splayset.empty Int.compare),
	succ = Splaymap.insert(succ,maxNumNodes + 1,Splayset.empty Int.compare)}

fun isNode ({maxNumNodes,pred,succ}:graph) node =
  let 
    val a = node > 0 andalso node <= (maxNumNodes) (* Compruebo que este entre 0 y maxNumNodes del grafo *)
    val b = case Splaymap.peek(pred,node) of (* Compruebo que exista en el diccionario de predecesores *)
              SOME _ => true
              | NONE => false
    val c = case Splaymap.peek(succ,node) of (* Compruebo que exista en el diccionario de sucesores *)
              SOME _ => true
              | NONE => false
  in if a andalso b andalso c then true else false
  end

fun isEdge ({succ,...}:graph) {from, to} = case Splaymap.peek(succ, from) of
																            SOME value => Splayset.member(value,to)
																            | NONE => raise notExistNode

fun isNotEdge ({succ,...}:graph) {from, to} = case Splaymap.peek(succ, from) of
																	              SOME value => (case Splayset.member(value,to) of
																							              		true => false
																									              | false => true)
																	              | NONE => raise notExistNode

fun adj (g as {pred,succ,...}:graph) node = if isNode g node then Splayset.listItems(Splayset.union(Splaymap.find(pred,node),Splaymap.find(succ,node))) (* Chequear *)
										                        else raise notExistNode

fun mk_edge (g as {maxNumNodes,pred,succ}:graph) {from, to} = 
  if isEdge g {from=from, to=to}
  then g
	else {maxNumNodes = maxNumNodes,
	     pred = Splaymap.insert(pred,to,Splayset.add(Splaymap.find(pred,to),from)),
       succ = Splaymap.insert(succ,from,Splayset.add(Splaymap.find(succ,from),to))}

fun rm_edge (g as {maxNumNodes,pred,succ}:graph) {from, to} =
  if isNotEdge g {from=from, to=to}
  then g
  else {maxNumNodes = maxNumNodes,
       pred = Splaymap.insert(pred,to,Splayset.delete(Splaymap.find(pred,to),from)),
       succ = Splaymap.insert(succ,from,Splayset.delete(Splaymap.find(succ,from),to))}

fun nodename node = Int.toString node

end