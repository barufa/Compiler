structure tigercolor :> tigercolor = 
struct

type allocation = (tigertemp.temp,tigerframe.register) Splaymap.dict

fun color instr{interference,initial,spillCost,registers} =
  let
    fun edgeCmp (t1,t2) (t3,t4) = if (String.compare(t1,t2) = EQUAL) then String.compare(t3,t4) else String.compare(t1,t2)

    (* Estructuras de datos *)
    val precolored: = ref ()
    val initial: tigertemp.temp Splayset.set = ref (Splayset.empty String.compare)
    val simplifyWorklist: tigertemp.temp Splayset.set = ref (Splayset.empty String.compare)
    val freezeWorklist: tigertemp.temp Splayset.set = ref (Splayset.empty String.compare)
    val spillWorklist: tigertemp.temp Splayset.set = ref (Splayset.empty String.compare)
    val spilledNodes: tigertemp.temp Splayset.set = ref (Splayset.empty String.compare)
    val coalescedNodes: tigertemp.temp Splayset.set = ref (Splayset.empty String.compare)
    val coloredNodes: tigertemp.temp Splayset.set = ref (Splayset.empty String.compare)
    val selectStack: tigertemp.temp tigerpila.Pila = tigerpila.nuevaPila()
      (* Se agrega esta estructura para poder calcular todos los elementos del stack *)
    val selectStackSet: tigertemp.temp Splayset.set = ref (Splayset.empty String.compare)

    val coalescedMoves: (tigertemp.temp * tigertemp.temp) Splayset.set = ref (Splayset.empty edgeCmp)
    val constrainedMoves: (tigertemp.temp * tigertemp.temp) Splayset.set = ref (Splayset.empty edgeCmp)
    val frozenMoves: (tigertemp.temp * tigertemp.temp) Splayset.set = ref (Splayset.empty edgeCmp)
    val worklistMoves: (tigertemp.temp * tigertemp.temp) Splayset.set = ref (Splayset.empty edgeCmp)
    val activeMoves: (tigertemp.temp * tigertemp.temp) Splayset.set = ref (Splayset.empty edgeCmp)

    val adjSet: tigertemp.temp * tigertemp.temp Splayset.set = ref (Splayset.empty edgeCmp)
    val adjList: (tigertemp.temp,tigertemp.temp Splayset.set) Splaymap.dict = ref (Splaymap.mkDict String.compare)
    val degree: (tigertemp.temp,int) Splaymap.dict = ref (Splaymap.mkDict String.compare)
    val moveList: (tigertemp.temp,(tigertemp.temp * tigertemp.temp) Splayset.set) Splaymap.dict = ref (Splaymap.mkDict String.compare)
    val alias: (tigertemp.temp,tigertemp.temp) Splaymap.dict = ref (Splaymap.mkDict String.compare)
    val color : (tigertemp.temp,tigerframe.register) Splaymap dict = ref (Splaymap.mkDict String.compare)

    val K: int = Splayset.numItems precolored

    (* Funciones *)
      (* Funciones auxiliares*)
    fun GetDegree n = case Splaymap.peek(!degree,n) of
                       SOME v => v
                       | NONE => 0

    fun GetAdjList n = case Splaymap.peek(!adjList,n) of
                        SOME v => v
                        | NONE => Splayset.empty String.compare

    fun GetMoveList n = case Splaymap.peek(!moveList,n) of
                          SOME v => v
                          | NONE => Splayset.empty edgeCmp

    fun PushSelectStack n = tigerpila.pushPila selectStack n;selectStackSet := Splayset.add(!selectStackSet,n)

    fun PopSelectStack =
      let
        val n = tigerpila.topPila selectStack
      in tigerpila.popPila selectStack;selectStackSet := Splayset.delete(!selectStackSet,n);n end 

    fun getItem set = case (Splayset.find (fn _ => true) set) of
                        SOME n => n
                        | NONE => raise Fail ("tigercolor.sml: el conjunto esta vacio")

    fun GetColor n = case (Splaymap.peek(!color,n)) of
                      SOME c => c
                      | NONE => raise Fail "tigercolor.sml: este temporario no tiene color asignado"

      (* Funciones del libro *)
    fun AddEdge (u,v) =
      if ((not(Splayset.member(!adjSet,(u,v)))) andalso (not(String.compare (u,v) = EQUAL)))
      then (adjSet := Splayset.add(!adjSet,(u,v));
            adjSet := Splayset.add(!adjSet,(v,u));
            if (not(Splayset.member(precolored,u)))
            then (adjList := Splaymap.insert(!adjList,u,(Splayset.add((GetAdjList u),v)));
                  degree := Splaymap.insert(!degree,u,((GetDegree u) + 1)))
            else ();
           if (not(Splayset.member(!precolored,v)))
            then (adjList := Splaymap.insert(!adjList,v,(Splayset.add((GetAdjList v),u)));
                  degree := Splaymap.insert(!degree,v,((GetDegree v) + 1)))
            else ())
      else ()

    fun Build () =

    fun NodeMoves n = Splayset.intersection(GetMoveList n,Splayset.union(!activeMoves,!worklistMoves))

    fun MoveRelated n = not(Splayset.isEmpty(NodeMoves n))

    fun MakeWorklist () = Splayset.app (fn n => initial := Splayset.delete(!initial,n);
                                                if (GetDegree n >= K)
                                                then (spillWorklist := Splayset.add(!spillWorklist,n))
                                                else(if (MoveRelated n)
                                                     then (freezeWorklist := Splayset.add(!freezeWorklist,n))
                                                     else (simplifyWorklist := Splayset.add(!simplifyWorklist,n)))) !initial

    fun Adjacent n = Splayset.intersection(GetAdjList n,Splayset.union(!selectStackSet,!coalescedNodes))

    fun EnableMoves nodes = Splayset.app (fn n => Splayset.app (fn m => if (Splayset.member(!activeMoves,m)) then (activeMoves := Splayset.delete(!activeMoves,m);worklistMoves := Splayset.add(!worklistMoves,m)) else ()) (NodeMoves n)) nodes

    fun DecrementDegree m = 
      let
        val d = GetDegree m
      in 
        degree := Splaymap.insert(!degree,m,d-1);
        if (d=K)
        then (EnableMoves(Splayset.add(Adjacent m,m));
              spillWorklist := Splayset.delete(!spillWorklist,m);
              if (MoveRelated m)
              then (freezeWorklist := Splayset.add(!freezeWorklist,m))
              else (simplifyWorklist := Splayset.add(!simplifyWorklist,m)))
        else ()
      end

    fun Simplify () = 
      let
        val n = getItem !simplifyWorklist
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
        j = ref 0
      in
        Splayset.app (fn n => if (GetDegree n >= K) then (j := !j + 1) else ()) nodes;!j < K
      end

    fun GetAlias n = if (Splayset.member(!coalescedNodes,n))
                     then GetAlias(Splaymap.find(!alias,n))
                     else n

    fun Combine (u,v) = if (Splayset.member(!freezeWorklist,v))
                        then (freezeWorklist := Splayset.delete(!freezeWorklist,v))
                        else (spillWorklist := Splayset.delete(!spillWorklist,v));
                        coalescedNodes := Splayset.add(!coalescedNodes,v);
                        alias := Splaymap.insert(!alias,v,u);
                        moveList := Splaymap.insert(!moveList,u,Splayset.union(Splaymap.find(!moveList,u),Splaymap.find(!moveList,v)));
                        Splayset.app (fn t => AddEdge (t,u);DecrementDegree t) (Adjacent v);
                        if (GetDegree u >= K andalso Splayset.member(!freezeWorklist,u))
                        then (freezeWorklist := Splayset.delete(!freezeWorklist,u);
                              spillWorklist := Splayset.add(spillWorklist,u))
                        else ()

    fun Coalesce () = 
      let
        val m = getItem !worklistMoves
        val (x,y) = m
        val x = GetAlias x
        val y = GetAlias y
        val (u,v) = if Splayset.member(!precolored,y) then (y,x) else (x,y)
      in
        worklistMoves := Splayset.delete(!worklistMoves,m);
        if (String.compare(u,v) = EQUAL)
        then (coalescedMoves := Splayset.add(!coalescedMoves,m);
              AddWorkList u;)
        else (if (Splayset.member(!precolored,v) orelse Splayset.member(!adjSet,(u,v)))
              then (constrainedMoves := Splayset.add(!constrainedMoves,m);
                    AddWorkList u;
                    AddWorkList v)
              else (if ((Splayset.member(!precolored,u) andalso (foldr (fn (t,b) => OK(t,u) andalso b ) true (Adjacent v))) orelse (not((Splayset.member(!precolored,u)) andalso Conservative(Splayset.union(Adjacent u,Adjacent v))))
                    then (coalescedMoves := Splayset.add(!coalescedMoves,m);
                          Combine(u,v);
                          AddWorkList u)
                    else (activeMoves := Splayset.add(!activeMoves,m))));
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
                    then (freezeWorklist := Splayset.detele(!freezeWorklist,v);
                          simplifyWorklist := Splaysetadd(!simplifyWorklist,v))
                    else ()
                  end
      in Splayset.app f (NodeMoves u)

    fun Freeze () =
      let
        val u = getItem !freezeWorklist
      in
        freezeWorklist := Splayset.delete(!freezeWorklist,u);
        simplifyWorklist := Splayset.add(!simplifyWorklist,u);
        FreezeMoves u
      end

    fun SelectSpill () =
      let
        val m = GetItem !spillWorklist
      in
        spillWorklist := Splayset.delete(!spillWorklist,m);
        simplifyWorklist := Splayset.add(!simplifyWorklist,m);
        FreezeMoves(m)
      end

    fun AssignColors () = 
      while (tigerpila.lenPila selectStack > 0)
      do (let
            val n = PopSelectStack ()
            val okColors = ref (Splayset.addList(Splayset.empty String.compare,!registers))
          in
            Splayset.app (fn w => if Splayset.member(Splayset.union(!coloredNodes,!precolored),GetAlias w) then (okColors := Splayset.delete(!okColors,GetColor(GetAlias(w)))) else ()) (GetAdjList n);
            if Splayset.isEmpty(!okColors)
            then (spilledNodes := Splayset.add(!spilledNodes,n))
            else (coloredNodes := Splayset.union(!coloredNodes,n);
                  color := Splaymap.insert(!color,n,getItem !okColor));
          end);
      Splayset.app (fn n => color := Splaymap.insert(!color,n,GetColor(GetAlias(n)))) !coalescedNodes

    fun RewriteProgram () =

  in
    (* Funcion color *)
  end

end