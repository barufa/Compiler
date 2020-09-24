structure tigercolor :> tigercolor = 
struct

open tigerassem

type allocation = (tigertemp.temp,tigerframe.register) Splaymap.dict

fun color (instr, frame) =
  let
    val _ = print("\n#####################################\n")
    val _ = print("Coloreando\n")
    fun edgeCmp ((t1,t2),(t3,t4)) = if (String.compare(t1,t3) = EQUAL) then String.compare(t2,t4) else String.compare(t1,t3)

    (*  *)
    val interference: tigerliveness.igraph ref = ref ({graph=tigergraph.newGraph(), tnode=Splaymap.mkDict String.compare, gtemp=Splaymap.mkDict Int.compare, moves=[]})
    val liveOut: (tigergraph.node,tigertemp.temp Splayset.set) Splaymap.dict ref = ref (Splaymap.mkDict Int.compare)
    (* Implementamos una heuristica simple donde los temporarios originales del programa tienen costo 1 y
       los nuevos temporarios creados en RewriteProgram un costo de 2. *)
    val spillCost: (tigertemp.temp,int) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)
    val registers: tigerframe.register list = tigerframe.machineregs

    (* Estructuras de datos *)
    val precolored: tigerframe.register Splayset.set ref= ref (Splayset.addList(Splayset.empty String.compare,registers))
    val initial: tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)
    val simplifyWorklist: tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)
    val freezeWorklist: tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)
    val spillWorklist: tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)
    val spilledNodes: tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)
    val coalescedNodes: tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)
    val coloredNodes: tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)
    val selectStack: tigertemp.temp tigerpila.Pila = tigerpila.nuevaPila()
      (* Se agrega esta estructura para poder calcular todos los elementos del stack *)
    val selectStackSet: tigertemp.temp Splayset.set ref = ref (Splayset.empty String.compare)

    val coalescedMoves: (tigertemp.temp * tigertemp.temp) Splayset.set ref = ref (Splayset.empty edgeCmp)
    val constrainedMoves: (tigertemp.temp * tigertemp.temp) Splayset.set ref = ref (Splayset.empty edgeCmp)
    val frozenMoves: (tigertemp.temp * tigertemp.temp) Splayset.set ref = ref (Splayset.empty edgeCmp)
    val worklistMoves: (tigertemp.temp * tigertemp.temp) Splayset.set ref = ref (Splayset.empty edgeCmp)
    val activeMoves: (tigertemp.temp * tigertemp.temp) Splayset.set ref = ref (Splayset.empty edgeCmp)

    val adjSet: (tigertemp.temp * tigertemp.temp) Splayset.set ref = ref (Splayset.empty edgeCmp)
    val adjList: (tigertemp.temp,tigertemp.temp Splayset.set) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)
    val degree: (tigertemp.temp,int) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)
    val moveList: (tigertemp.temp,(tigertemp.temp * tigertemp.temp) Splayset.set) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)
    val alias: (tigertemp.temp,tigertemp.temp) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)
    val color: allocation ref = ref (Splayset.foldl (fn (n,dic) => Splaymap.insert(dic,n,n)) (Splaymap.mkDict String.compare) (!precolored))

    val K: int = Splayset.numItems (!precolored)

    (* Mapeamos temporales que se movieron a memoria a su direccion de memoria correspondiente.
       Luego en el .data se reservara espacio en memoria para estos valores. *)
    val spillTable: (tigertemp.temp,String.string) Splaymap.dict ref = ref (Splaymap.mkDict String.compare)

    (* Funciones *)
      (* Funciones auxiliares*)
    fun spillNewNode t =
      let
        fun spill () =
          let
            val n = Splaymap.numItems (!spillTable)
            val s = "spill"^Int.toString (n+1)
            val _ = spillTable := Splaymap.insert(!spillTable,t,s)
          in () end
      in
        case Splaymap.peek(!spillTable,t) of
          SOME v => ()
          | NONE => spill()
      end

    fun mapSpillNode t = Splaymap.find(!spillTable,t)

    fun GetDegree n = case Splaymap.peek(!degree,n) of
                       SOME v => v
                       | NONE => 0

    fun GetAdjList n = case Splaymap.peek(!adjList,n) of
                        SOME v => v
                        | NONE => Splayset.empty String.compare

    fun GetMoveList n = case Splaymap.peek(!moveList,n) of
                          SOME v => v
                          | NONE => Splayset.empty edgeCmp

    fun PushSelectStack n =  (tigerpila.pushPila selectStack n;selectStackSet := Splayset.add(!selectStackSet,n))

    fun PopSelectStack () =
      let
        val n = tigerpila.topPila selectStack
      in tigerpila.popPila selectStack;selectStackSet := Splayset.delete(!selectStackSet,n);n end 

    fun EmptySelectStack () = 
      while(tigerpila.lenPila selectStack > 0)
      do(tigerpila.popPila selectStack)

    fun getItem set = case (Splayset.find (fn _ => true) set) of
                        SOME n => n
                        | NONE => raise Fail ("tigercolor.sml: el conjunto esta vacio")

    fun stringToSet s = Splayset.singleton (String.compare) s

    fun GetColor n = case (Splaymap.peek(!color,n)) of
                      SOME c => c
                      | NONE => raise Fail "tigercolor.sml: este temporario no tiene color asignado"

    fun addSpillCost temp cost =
      case Splaymap.peek(!spillCost,temp) of
        SOME _ => ()
        | NONE => spillCost := Splaymap.insert(!spillCost,temp,cost)

    fun getSpillCost temp =
      case Splaymap.peek(!spillCost,temp) of
        SOME n => n
        | NONE => raise Fail ("El temporario "^temp^" no esta en spillCost")

    fun minSpillCost temps =
      let
        fun pertenece (x::xs) (t,c) = if String.compare(x,t) = EQUAL then true else pertenece xs (t,c)
        | pertenece [] _ = false
        val filter = List.filter (pertenece temps) (Splaymap.listItems (!spillCost))
        (* Asumo que el costo no va a llegar a 123456789 *)
        fun getMin ((t,c)::xs) _ (12346789) = getMin xs t c
        | getMin ((t,c)::xs) tmin cmin = if c < cmin then getMin xs t c else getMin xs tmin cmin
        | getMin [] tmin _ = tmin
      in getMin filter ("") (123456789) end

    fun getItemToSpill () =
      case Splayset.isEmpty(!spillWorklist) of
        false => minSpillCost (Splayset.listItems (!spillWorklist))
        | true => raise Fail ("spillWorklist esta vacio")

    fun getItemToSimplify () = 
      case Splayset.isEmpty(!simplifyWorklist) of
        false => minSpillCost (Splayset.listItems (!simplifyWorklist))
        | true => raise Fail ("simplifyWorklist esta vacio")

      (* Funciones del libro *)
    fun AddEdge (u,v) =
      if ((not(Splayset.member(!adjSet,(u,v)))) andalso (not(String.compare (u,v) = EQUAL)))
      then (adjSet := Splayset.add(!adjSet,(u,v));
            adjSet := Splayset.add(!adjSet,(v,u));
            if (not(Splayset.member(!precolored,u)))
            then (adjList := Splaymap.insert(!adjList,u,(Splayset.add((GetAdjList u),v)));
                  degree := Splaymap.insert(!degree,u,((GetDegree u) + 1)))
            else ();
            if (not(Splayset.member(!precolored,v)))
            then (adjList := Splaymap.insert(!adjList,v,(Splayset.add((GetAdjList v),u)));
                  degree := Splaymap.insert(!degree,v,((GetDegree v) + 1)))
            else ())
      else ()

    fun Build instrs =
      let
        fun getDst inst = case inst of
                             IOPER {assem,dst,src,jump} => Splayset.addList(Splayset.empty String.compare,dst)
                           | IMOVE {assem,dst,src} => Splayset.add(Splayset.empty String.compare,dst)
                           | _ => Splayset.empty String.compare
        fun getSrc inst = case inst of
                             IOPER {assem,dst,src,jump} => Splayset.addList(Splayset.empty String.compare,src)
                           | IMOVE {assem,dst,src} => Splayset.add(Splayset.empty String.compare,src)
                           | _ => Splayset.empty String.compare
        fun isMove inst = case inst of
                             IMOVE {...} => true
                           | _ => false
        fun aux (ILABEL{...},n) = ()
        | aux (inst,n) =
          let
            val def_I = getDst inst
            val use_I = getSrc inst
            val liveO_I = Splaymap.find(!liveOut,n)
          in
            if (isMove inst) (* Si es una instruccion move, def_I y use_I tienen un solo elemento *)
            then (
                  moveList := Splaymap.insert(!moveList,getItem def_I,Splayset.add(GetMoveList (getItem def_I),(getItem def_I,getItem use_I)));
                  moveList := Splaymap.insert(!moveList,getItem use_I,Splayset.add(GetMoveList (getItem use_I),(getItem def_I,getItem use_I)));
                  worklistMoves := Splayset.add(!worklistMoves,(getItem def_I,getItem use_I)))
            else ();
            Splayset.app (fn d => Splayset.app (fn l => AddEdge(l,d)) (liveO_I)) def_I
          end
          fun listToN a b = 
            case Int.compare(a,b) of
              EQUAL => [b]
              | LESS => a::listToN (a+1) b
              | GREATER => raise Fail "No deberia pasar"
          val _ = List.app (fn (i,n) => aux (i,n)) (ListPair.zip (instrs,listToN 1 (List.length instrs)))
      in () end

    fun NodeMoves n = Splayset.intersection(GetMoveList n,Splayset.union(!activeMoves,!worklistMoves))

    fun MoveRelated n = not(Splayset.isEmpty(NodeMoves n))

    fun MakeWorklist () = Splayset.app (fn n => let
                                                  val _ = initial := Splayset.delete(!initial,n)
                                                  val _ = if (GetDegree n >= K)
                                                          then (spillWorklist := Splayset.add(!spillWorklist,n))
                                                          else(if (MoveRelated n)
                                                               then (freezeWorklist := Splayset.add(!freezeWorklist,n))
                                                               else (simplifyWorklist := Splayset.add(!simplifyWorklist,n)))
                                                in () end) (!initial)

    fun Adjacent n = Splayset.difference(GetAdjList n,Splayset.union(!selectStackSet,!coalescedNodes))

    fun EnableMoves nodes = Splayset.app (fn n => Splayset.app (fn m => if (Splayset.member(!activeMoves,m)) then (activeMoves := Splayset.delete(!activeMoves,m);worklistMoves := Splayset.add(!worklistMoves,m)) else ()) (NodeMoves n)) nodes

    fun DecrementDegree m = 
      let
        val d = GetDegree m
      in
        degree := Splaymap.insert(!degree,m,d-1);
        if (d=K)
        then (EnableMoves(Splayset.add(Adjacent m,m));
              spillWorklist := Splayset.difference(!spillWorklist,stringToSet m);
              if (MoveRelated m)
              then (freezeWorklist := Splayset.add(!freezeWorklist,m))
              else (simplifyWorklist := Splayset.add(!simplifyWorklist,m)))
        else ()
      end

    fun Simplify () =
      let
        val n = getItemToSimplify()
        val _ = print("Simplify de "^n^"\n")
      in 
        simplifyWorklist := Splayset.delete(!simplifyWorklist,n);
        PushSelectStack n;
        Splayset.app (fn m => DecrementDegree m) (Adjacent n)
      end

    fun AddWorkList u = if (not(Splayset.member(!precolored,u)) andalso not(MoveRelated u) andalso GetDegree u < K)
                        then (freezeWorklist := Splayset.delete(!freezeWorklist,u);
                              simplifyWorklist := Splayset.add(!simplifyWorklist,u))
                        else ()

    fun OK (t,r) = GetDegree t < K orelse Splayset.member(!precolored,t) orelse Splayset.member(!adjSet,(t,r))

    fun Conservative nodes = 
      let
        val j = ref 0
      in
        Splayset.app (fn n => if (GetDegree n >= K) then (j := !j + 1) else ()) nodes;!j < K
      end

    fun GetAlias n = if (Splayset.member(!coalescedNodes,n))
                     then GetAlias(Splaymap.find(!alias,n))
                     else n

    fun Combine (u,v) =
      let
        val _ = print("Combinando ("^u^","^v^")\n")
        val _ = if (Splayset.member(!freezeWorklist,v))
                then (freezeWorklist := Splayset.delete(!freezeWorklist,v))
                else (spillWorklist := Splayset.delete(!spillWorklist,v))
        val _ = coalescedNodes := Splayset.add(!coalescedNodes,v)
        val _ = alias := Splaymap.insert(!alias,v,u)
        val _ = moveList := Splaymap.insert(!moveList,u,Splayset.union(Splaymap.find(!moveList,u),Splaymap.find(!moveList,v)))
        val _ = Splayset.app (fn t => let val _ = AddEdge (t,u) val _ = DecrementDegree t in () end) (Adjacent v)
        val _ = if (GetDegree u >= K andalso Splayset.member(!freezeWorklist,u))
                then (freezeWorklist := Splayset.delete(!freezeWorklist,u);
                      spillWorklist := Splayset.add(!spillWorklist,u))
                else ()
      in () end

    fun Coalesce () = 
      let
        val m = getItem (!worklistMoves)
        val (x,y) = m
        val _ = print("Coalesce de ("^x^","^y^")\n")
        val x = GetAlias x
        val y = GetAlias y
        val (u,v) = if Splayset.member(!precolored,y) then (y,x) else (x,y)
      in
        worklistMoves := Splayset.delete(!worklistMoves,m);
        if (String.compare(u,v) = EQUAL)
        then (coalescedMoves := Splayset.add(!coalescedMoves,m);
              AddWorkList u)
        else (if (Splayset.member(!precolored,v) orelse Splayset.member(!adjSet,(u,v)))
              then (constrainedMoves := Splayset.add(!constrainedMoves,m);
                    AddWorkList u;
                    AddWorkList v)
              else (if ((Splayset.member(!precolored,u) andalso (Splayset.foldr (fn (t,b) => OK(t,u) andalso b ) true (Adjacent v))) orelse (not((Splayset.member(!precolored,u)) andalso Conservative(Splayset.union(Adjacent u,Adjacent v)))))
                    then (coalescedMoves := Splayset.add(!coalescedMoves,m);
                          Combine(u,v);
                          AddWorkList u)
                    else (activeMoves := Splayset.add(!activeMoves,m))))
      end

    fun FreezeMoves u =
      let 
        fun f m = let
                    val (x,y) = m
                    val v = if (String.compare(GetAlias y,GetAlias u) = EQUAL) then (GetAlias x) else (GetAlias y)
                  in
                    activeMoves := Splayset.delete(!activeMoves,m);
                    frozenMoves := Splayset.add(!frozenMoves,m);
                    if (Splayset.isEmpty(NodeMoves v) andalso (GetDegree v) < K)
                    then (freezeWorklist := Splayset.delete(!freezeWorklist,v);
                          simplifyWorklist := Splayset.add(!simplifyWorklist,v))
                    else ()
                  end
      in Splayset.app f (NodeMoves u) end

    fun Freeze () =
      let
        val u = getItem (!freezeWorklist)
        val _ = print("Freeze de "^u^"\n")
      in
        freezeWorklist := Splayset.delete(!freezeWorklist,u);
        simplifyWorklist := Splayset.add(!simplifyWorklist,u);
        FreezeMoves u
      end

    fun SelectSpill () =
      let
        val m = getItemToSpill()
        val _ = print("SelectSpill de "^m^"\n")
      in
        spillWorklist := Splayset.delete(!spillWorklist,m);
        simplifyWorklist := Splayset.add(!simplifyWorklist,m);
        FreezeMoves(m)
      end

    fun AssignColors () =
      let
        val _ = print("Asignando colores\n")
        fun aux () =
          let
            val n = PopSelectStack ()
            val okColors = ref (Splayset.addList(Splayset.empty String.compare,registers))
            val _ = Splayset.app (fn w => if Splayset.member(Splayset.union(!coloredNodes,!precolored),GetAlias w) then (okColors := Splayset.difference(!okColors,stringToSet (GetColor(GetAlias(w))))) else ()) (GetAdjList n)
            val _ = if Splayset.isEmpty(!okColors)
                    then (print("Moviendo \""^n^"\" a spilledNodes\n");
                          spilledNodes := Splayset.add(!spilledNodes,n))
                    else (print("Asignandole un color a \""^n^"\"\n");
                          coloredNodes := Splayset.add(!coloredNodes,n);
                          color := Splaymap.insert(!color,n,getItem (!okColors))) 
          in () end
        fun colorCoalesced n = if not(Splayset.member(!spilledNodes,GetAlias n))
                               then (color := Splaymap.insert(!color,n,GetColor(GetAlias(n))))
                               else ()
      in
        while (tigerpila.lenPila selectStack > 0)
        do (aux());
        Splayset.app colorCoalesced (!coalescedNodes)
      end

    fun RewriteProgram instrs =
      let
        val _ = print("Reescribiendo programa\n")
        (* Cambia todas las apariciones del temporario temp por nuevos temporarios *)
        fun addNewTemps (IOPER{assem,dst,src,jump}::inst) temp (ilist,dlist,ulist) = 
          let
            val (dlist,dst,ulist,src) =
              case (List.exists (fn n => String.compare(n,temp) = EQUAL) dst,List.exists (fn n => String.compare(n,temp) = EQUAL) src) of
                (true,true) => let val t = tigertemp.newtemp() in (t::dlist,List.map (fn t' => if String.compare(t',temp)=EQUAL then t else t') dst,t::ulist,List.map (fn t' => if String.compare(t',temp)=EQUAL then t else t') src) end
                | (true,false) => let val t = tigertemp.newtemp() in (t::dlist,List.map (fn t' => if String.compare(t',temp)=EQUAL then t else t') dst,ulist,src) end
                | (false,true) => let val t = tigertemp.newtemp() in (dlist,dst,t::ulist,List.map (fn t' => if String.compare(t',temp)=EQUAL then t else t') src) end
                | (false,false) => (dlist,dst,ulist,src)
            val ilist = IOPER{assem=assem,dst=dst,src=src,jump=jump}::ilist
          in addNewTemps inst temp (ilist,dlist,ulist) end
        | addNewTemps (IMOVE{assem,dst,src}::inst) temp (ilist,dlist,ulist) = 
          let
            val (dlist,dst,ulist,src) =
              case (String.compare(dst,temp),String.compare(src,temp)) of
                (EQUAL,EQUAL) => let val t = tigertemp.newtemp() in (t::dlist,t,t::ulist,t) end
                | (EQUAL,_) => let val t = tigertemp.newtemp() in (t::dlist,t,ulist,src) end
                | (_,EQUAL) => let val t = tigertemp.newtemp() in (dlist,dst,t::ulist,t) end
                | _ => (dlist,dst,ulist,src)
            val ilist = IMOVE{assem=assem,dst=dst,src=src}::ilist
          in addNewTemps inst temp (ilist,dlist,ulist) end
        | addNewTemps (i::inst) temp (ilist,dlist,ulist) = addNewTemps inst temp (i::ilist,dlist,ulist)
        | addNewTemps [] temp (ilist,dlist,ulist) = (List.rev ilist,dlist,ulist)
        (* Agrega las instrucciones de almacenar en la memoria reservada para temp los nuevos
           temporarios que se agregaron con la funcion addNewTemps *)
        fun addStore (IOPER{assem,dst,src,jump}::inst) temp ilist dlist = 
          let
            val a = Splayset.intersection(Splayset.addList(Splayset.empty String.compare,dst),Splayset.addList(Splayset.empty String.compare,dlist))
            val _ = if Splayset.numItems(a) > 1 then print("tigercolor: Chequear. Tiene mas de 2 elementos.\n") else ()
            val t' = if Splayset.numItems(a) > 0 then getItem a else ""
            val l = if Splayset.numItems(a) > 0 then [IOPER{assem="movq %'s0, "^mapSpillNode temp^"",src=[t'],dst=[],jump=NONE},IOPER{assem=assem,dst=dst,src=src,jump=jump}] else [IOPER{assem=assem,dst=dst,src=src,jump=jump}]
            val dlist = if Splayset.numItems(a) > 0 then List.filter (fn n => not(String.compare(n,t') = EQUAL)) dlist else dlist
            val ilist = l @ ilist
          in addStore inst temp ilist dlist end
        | addStore (IMOVE{assem,dst,src}::inst) temp ilist dlist = 
          let
            val l = if List.exists (fn n => String.compare(n,dst) = EQUAL) dlist then [IOPER{assem="movq %'s0, "^mapSpillNode temp^"",src=[dst],dst=[],jump=NONE},IMOVE{assem=assem,dst=dst,src=src}] else [IMOVE{assem=assem,dst=dst,src=src}]
            val dlist = if List.exists (fn n => String.compare(n,dst) = EQUAL) dlist then List.filter (fn n => not(String.compare(n,dst) = EQUAL)) dlist else dlist
            val ilist = l @ ilist
          in addStore inst temp ilist dlist end
        | addStore (i::inst) temp ilist dlist = addStore inst temp (i::ilist) dlist
        | addStore [] temp ilist dlist = List.rev ilist
        (* Agrega las instrucciones de cargar el valor de temp desde la memoria a los nuevos
           temporarios que se agregaron con la funcion addNewTemps *)
        fun addLoad (IOPER{assem,dst,src,jump}::inst) temp ilist ulist = 
          let
            val a = Splayset.intersection(Splayset.addList(Splayset.empty String.compare,src),Splayset.addList(Splayset.empty String.compare,ulist))
            val _ = if Splayset.numItems(a) > 1 then print("tigercolor: Chequear. Tiene mas de 2 elementos.\n") else ()
            val t' = if Splayset.numItems(a) > 0 then getItem a else ""
            val l = if Splayset.numItems(a) > 0 then [IOPER{assem=assem,dst=dst,src=src,jump=jump},IOPER{assem="movq "^mapSpillNode temp^", %'d0",src=[],dst=[t'],jump=NONE}] else [IOPER{assem=assem,dst=dst,src=src,jump=jump}]
            val ulist = if Splayset.numItems(a) > 0 then List.filter (fn n => not(String.compare(n,t') = EQUAL)) ulist else ulist
            val ilist = l @ ilist
          in addLoad inst temp ilist ulist end
        | addLoad (IMOVE{assem,dst,src}::inst) temp ilist ulist = 
          let
            val l = if List.exists (fn n => String.compare(n,src) = EQUAL) ulist then [IMOVE{assem=assem,dst=dst,src=src},IOPER{assem="movq "^mapSpillNode temp^", %'d0",src=[],dst=[src],jump=NONE}] else [IMOVE{assem=assem,dst=dst,src=src}]
            val ulist = if List.exists (fn n => String.compare(n,src) = EQUAL) ulist then List.filter (fn n => not(String.compare(n,src) = EQUAL)) ulist else ulist
            val ilist = l @ ilist
          in addLoad inst temp ilist ulist end
        | addLoad (i::inst) temp ilist ulist = addLoad inst temp (i::ilist) ulist
        | addLoad [] temp ilist ulist = List.rev ilist
        (* Agrega las nuevas instrucciones de carga y guardado cuando se derrama un nodo 
           Tener en cuenta que primero se debe aplicar addStore antes que addLoad, sino generara un codigo erroneo *)
        fun proccess (temp,inst) =
          let
            val (ilist,dlist,ulist) = addNewTemps inst temp ([],[],[])
            (* Agregamos los nuevos temporarios a la funcion spillCost con costo 2(es el costo mayor en este caso) *)
            val _ = List.app (fn t => addSpillCost t 2) (dlist @ ulist)
            val ilist = addStore ilist temp [] dlist
            val ilist = addLoad ilist temp [] ulist
          in ilist end
        (* Agregamos los temporarios que se van a mover a memoria a la tabla de temporarios derramados *)
        val _ = List.app (fn t => spillNewNode t) (Splayset.listItems (!spilledNodes))
        (* Modificamos las instrucciones *)
        val instrs = List.foldl proccess instrs (Splayset.listItems (!spilledNodes))
      in
        instrs
      end

    fun LivenessAnalysis instrs =
      let
        val _ = print("Calculando liveness analysis\n")
        val (fgraph,nodes) = tigerflow.instr2graph instrs
        val (ig,lo) = tigerliveness.interferenceGraph fgraph
        val {control, def, use, ismove} = fgraph
        val _ = interference := ig
        val _ = liveOut := lo
        (* Calculamos todos los temporarios que se usan en instrs *)
        val temps = List.foldl (fn (n,set) => Splayset.addList(set,Splaymap.find(def,n) @ Splaymap.find(use,n))) (Splayset.empty String.compare) nodes
        (* Eliminamos de temps los temporarios precoloreados *)
        val temps = Splayset.difference(temps,!precolored)
        (* Inicializamos initial con los temporales*)
        val _ = Splayset.app (fn t => initial := Splayset.add(!initial,t)) temps
        (* Agregamos los temporales a spillCost con costo 1.
           Si ya estan en spillCost no se modifica para no pisar los costos en las llamadas recursivas. *)
        val _ = Splayset.app (fn t => addSpillCost t 1) temps
      in () end

    (* Init inicializa todas la variables para cuando se vuelva a llamar main
    despues de reescribir el programa estas esten igual que como se llamo a
    la funcion color *)
    fun Init () =
      let
        val _ = liveOut := Splaymap.mkDict Int.compare
        val _ = initial := Splayset.empty String.compare
        val _ = simplifyWorklist := Splayset.empty String.compare
        val _ = freezeWorklist := Splayset.empty String.compare
        val _ = spillWorklist := Splayset.empty String.compare
        val _ = spilledNodes := Splayset.empty String.compare
        val _ = coalescedNodes := Splayset.empty String.compare
        val _ = coloredNodes := Splayset.empty String.compare
        (* Vacio la pila en vez de crear una nueva vacia *)
        val _ = EmptySelectStack()
        val _ = selectStackSet := Splayset.empty String.compare
        val _ = coalescedMoves := Splayset.empty edgeCmp
        val _ = constrainedMoves := Splayset.empty edgeCmp
        val _ = frozenMoves := Splayset.empty edgeCmp
        val _ = worklistMoves := Splayset.empty edgeCmp
        val _ = activeMoves := Splayset.empty edgeCmp
        val _ = adjSet := Splayset.empty edgeCmp
        val _ = adjList := Splaymap.mkDict String.compare
        val _ = degree := Splaymap.mkDict String.compare
        val _ = moveList := Splaymap.mkDict String.compare
        val _ = alias := Splaymap.mkDict String.compare
        val _ = color := Splayset.foldl (fn (n,dic) => Splaymap.insert(dic,n,n)) (Splaymap.mkDict String.compare) (!precolored)
      in () end

    fun Main instrs = 
      let
        val _ = Init()
        val _ = LivenessAnalysis instrs
        val _ = Build instrs
        val _ = MakeWorklist()
        val _ = while ((not (Splayset.isEmpty (!simplifyWorklist))) orelse (not (Splayset.isEmpty (!worklistMoves))) orelse (not (Splayset.isEmpty  (!freezeWorklist))) orelse (not (Splayset.isEmpty (!spillWorklist))))
                do (if (not (Splayset.isEmpty (!simplifyWorklist))) then Simplify()
                    else if (not (Splayset.isEmpty (!worklistMoves))) then Coalesce()
                    else if (not (Splayset.isEmpty (!freezeWorklist))) then Freeze()
                    else if (not (Splayset.isEmpty (!spillWorklist))) then SelectSpill()
                    else ())
        val _ = AssignColors()
        val inst = if (not (Splayset.isEmpty (!spilledNodes)))
                   then Main(RewriteProgram instrs)
                   else (instrs)
      in
        inst
      end

    val instrucciones = Main instr
    val _ = print("Fin del coloreo\n")
    val _ = print("#####################################\n")
  in
    (instrucciones,!color,!spillTable)
  end

  fun debug (instrs,alloc,table) =
    let
          val _ = print("#################################\n")
          val _ = print("Mostrando las instrucciones:\n")
          fun printInstr i = tigerassem.formatCode i
          val _ = List.app printInstr instrs
          val _ = print("#################################\n")
          val _ = print("Mostrando la informacion de la spillTable:\n")
          fun printItem (a,b) = print(a^" --> "^b^"\n")
          val _ = List.app printItem (Splaymap.listItems table)
          val _ = print("#################################\n")
          val _ = print("Mostrando el resultado de la allocacion\n")
          fun printAlloc (t,r) = print(t^" --> "^r^"\n")
          val _ = List.app printAlloc (Splaymap.listItems alloc)
          val _ = print("#################################\n")
    in () end

end
