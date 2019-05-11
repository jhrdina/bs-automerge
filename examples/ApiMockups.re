/** Various API mockups, not indended for compilation. */;

/* ------------- */
/* a) Imperative */
/* ------------- */

/*
 module AM = Automerge;
 let d = AM.init();
 let d = d->AM.change("Something", d => {...d, a: "aval"});

 /* Functional cursors */

 let d =
   d->AM.change("Something", json => {
     let bCur = AM.root->AM.cur("B");
     json
     |> AM.set(AM.root, AM.EmptyObject)
     |?> AM.set(AM.root->AM.cur("Asdf"), AM.String("asdf"))
     |?> AM.set(bCur, AM.EmptyArray)
     |?> AM.prepend(bCur, AM.Int(42))
     |?> AM.prepend(bCur, AM.EmptyObject)
     |?> AM.set(bCur->AM.curI(1), AM.Int(43));
   });

 /* Array cursors
    [BAD] It is impossible/hard to reuse parts of cursor */
 let d =
   d->AM.change("Something", json =>
     json
     |> AM.set([], AM.EmptyObject)
     |?> AM.set([K("Asdf")], AM.String("asdf"))
     |?> AM.set([K("B")], AM.EmptyArray)
     |?> AM.prepend([K("B")], AM.Int(42))
     |?> AM.prepend([K("B")], AM.EmptyObject)
     |?> AM.set([K("B"), I(1)], AM.Int(43))
   );

 let maybeVal = d |> AM.ctx |> AM.getString([K("Asdf")]);
 let maybeArray = d |> AM.ctx |> AM.getArray([K("B")]);
 let maybeResult =
   maybeArray
   |?> AM.Array.fold_left((acc, thing) => thing |> AM.Array.getInt(), []);

 let d =
   d->AM.change("Something", (curHist: (cursor, doc)) =>
     curHist |?> assign() |?> Obj.get("Asdf")
   );

 /* Update functions [BROKEN] */

 let d =
   d->AM.change("Something", d =>
     d
     |> AM.update("Asdf", _ => AM.String("asdf"))
     |> AM.update("B", bCur => AM.EmptyArray |> AM.update(bCur))
   );

 /* Ops as data
      [GOOD~] Useful as generation target for diffing
      [BAD] Branching would be hell
    */

 let d =
   d->AM.change("Something", json => {
     let bCur = AM.root->AM.cur("B");
     [
       AM.Set(AM.root, AM.EmptyObject),
       AM.Set(AM.root->AM.cur("Asdf"), AM.String("asdf")),
       AM.Set(bCur, AM.EmptyArray),
       AM.Prepend(bCur, AM.Int(42)),
       AM.Prepend(bCur, AM.EmptyObject),
       AM.Set(bCur->AM.curI(1), AM.Int(43)),
     ];
   });

 /* Objects with UUIDs */

 let d =
   d->AM.change("Something", d => {
     let rootObj = AM.Obj.create();
     let bArray = AM.Arr.create();
     d
     |?> AM.Obj.add("Asdf", AM.String("asdf"), d.root)
     |?> AM.Obj.add("B", bArray, d.root)
     |?> AM.Arr.prepend(AM.Int(42), bArray)
     |?> AM.Arr.prepend(AM.Obj.create(), bArray)
     |?> AM.Arr.set(1, AM.Int(43), bArray);
   });

 let maybeVal = d |> root |> AM.Obj.get("Asdf") |?> AM.decodeString;
 let maybeArray =
   d
   |> root
   |> AM.Obj.get("B")
   |?> AM.Arr.foldLeft(
         (acc, item) =>
           switch (item |> AM.decodeInt) {
           | Some(i) => [i, ...acc]
           | None => acc
           },
         [],
       );
 let maybeKeyValArray =
   AM.Obj.fold(
     (key, value, acc) =>
       switch (value |> AM.decodeString) {
       | Some(str) => [(key, str), ...acc]
       | None => acc
       },
     d |> root,
     [],
   );

 /* --------------------------------------------- */
 /* b) With diffing                               */
 /* --------------------------------------------- */

 /* Direct manipulation, universal types */

 let d =
   d->AM.change("Something", json => {
     let stateA =
       Json.Object([
         ("Asdf", Json.String("asdf")),
         ("B", Json.Array([Json.Object([]), Json.Int(42)])),
       ]);
     /* This crazy blob is equivalent of `root.B[1]=42` */
     switch (stateA) {
     | Json.Object(items) =>
       Json.Object(
         items
         |> List.rev_map(((k, v)) =>
              k === "B"
                ? switch (v) {
                  | Json.Array(items) =>
                    Json.Array(
                      List.fold_right(
                        (v, (i, acc)) =>
                          [i === 1 ? Json.Int(42) : v, ...acc],
                        items,
                        (0, []),
                      )
                      |> snd,
                    )
                  | _ => Json.Array([])
                  }
                : v
            ),
       )
     };
     /* TODO: Rewrite using update/replace methods */
   });

 /* Helper functions, universal types << TOP TOP TOP!! */

 let d =
   d->AM.change("Something", json => {
     open AM;
     let stateA =
       Obj.empty
       |> Obj.add("Asdf", String("asdf"))
       |?> Obj.add(
             "B",
             Arr.prepend(Obj.empty, Arr.prepend(Int(42), Arr.empty)),
           );

     stateA
     |?> Obj.add(
           "B",
           stateA |?> Obj.findOpt("B") |?> Arr.updateAt(1, Int(43)),
         );

     /* Alt. */
     stateA |?> Obj.update("B", Arr.updateAt(1, Int(43)));

     /* Conflicts */
     stateA
     |?> Obj.update("B", Reg.resolveConflicts(values => List.hd(values)));
   });

 let conflicts = d->ctx |> Obj.findOpt("B") |?> Reg.values;
 /* present to User */
 let resolvedValue = conflicts |> List.first;
 let d =
   d->AM.change("Resolve", json =>
     AM.
       /* Conflicts */
       (
         json
         |?> Obj.update("B", Reg.resolveConflicts(conflicts, resolvedValue))
       )
   );

 /* Schema via direct manipulation */

 type schemaMember =
   /* bool = needed */
   | Object(list((string, bool, schemaMember)))
   | String
   | Int
   | Array;

 let userSchema =
   AM.Object([
     ("id", true, AM.String),
     ("name", true, AM.String),
     ("items", true, AM.Array(AM.String)),
   ]);

 let name = root |> AM.getObj("me") |> AM.getStr("name");

 /* Problem: how to get data easily with type information? */

 /* Schema via methods */
 let userSchema = AM.Schema.Obj.empty |> add("id", objectSchema);

 /* let rootSchema = */

 /* Schema via modules */

 /* module User = */

 module UserSchema = {
   let id = AM.string("id", true);
   let name = AM.string("name", true);
   let items = AM.array("items", true, AM.string);

   let schema = AM.Obj.Schema.(empty |> add(id) |> add(name) |> add(items));
 };

 module RootSchema = {
   let me = AM.object_((module UserSchema));
 };

 module AM = {
   let string = (name, required, o) => o |> AM.Obj.get();
 };

 let name = root |> AM.Obj.get(RootSchema.me) |> AM.Obj.get(UserSchema.name);

 let schema = AM.Object([("me", true, AM.Object([()]))]);

 */