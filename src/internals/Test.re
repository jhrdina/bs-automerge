open InfixRe;

/* ================= */
/* Tight JS API demo */
/* ================= */

module AM = Automerge.Js;

let d1 =
  AM.(
    initWithActorId("aaaaaaaa")
    ->change("test", root => {
        root->Js.Dict.set(
          "ufoo",
          Js.Json.(
            array([|
              number(42.0),
              number(20.0),
              number(11.0),
              number(19.0),
            |])
          ),
        );

        root->Js.Dict.set("asdf", Js.Json.string("asdf"));
      })
  );

let d2a =
  AM.(
    initWithActorId("bbbbbbbb")
    ->merge(d1)
    ->change("get crazy", d => {
        d->Js.Dict.set("asdf", Js.Json.string("paulus"));
        switch (d->Js.Dict.get("ufoo") |?> Js.Json.decodeArray) {
        | Some(ufoo) => ufoo[2] = Js.Json.number(15.0)
        | None => ()
        };
      })
  );

let d2b =
  AM.(
    d1->change("get dizzy", d => {
      d->Js.Dict.set("asdf", Js.Json.string("omangus"));
      switch (d->Js.Dict.get("ufoo") |?> Js.Json.decodeArray) {
      | Some(ufoo) => ufoo[2] = Js.Json.number(9.0)
      | None => ()
      };
    })
  );

let merged = AM.merge(d2a, d2b);

let clockForRemote = d2a->AM.getClock;
let changesFromRemote = d2b->AM.getMissingChanges(clockForRemote);
let merged2 = d2a->AM.applyChanges(changesFromRemote);

switch (merged->Js.Dict.get("ufoo") |?> Js.Json.decodeArray) {
| Some(ufoo) =>
  let conflicts = ufoo->AM.getArrayConflicts;
  Js.log(conflicts);
  ();
| None => ()
};
Js.log(merged);
Js.log("===========");
Js.log(merged2);

/* DREAM API */

type inbound = {guvno: int};

type doc = {
  a: string,
  b: int,
  c: inbound,
};

type t = {sdf: string};

type someType =
  | SomeType('t): someType;
type db = {a: int};
type local = {b: int};
module StrMap = Map.Make(String);
let mm =
  StrMap.(
    empty |> add("db", SomeType({a: 1})) |> add("local", SomeType({b: 2}))
  );

/* ================ */
/* Unified API demo */
/* ================ */

module UniAM = Automerge.UniJs;

let d1 =
  UniAM.(
    make(ActorId.ofString("aaaaaaaa"))
    |> change("test", root =>
         Json.(
           root
           |> Map.add(
                "ufoo",
                List.(
                  create()
                  |> prepend(int(19))
                  |> prepend(int(11))
                  |> prepend(int(20))
                  |> prepend(int(42))
                  |> toJson
                ),
              )
           |> Map.add("asdf", string("asdf"))
         )
       )
  );

let merge = (d1, d2) =>
  UniAM.(d2 |> applyChanges(d1 |> getChangesFromTime(d2 |> getClock)));

let d2a =
  UniAM.(
    make(ActorId.ofString("bbbbbbbb"))
    |> merge(d1)
    |> change("get crazy", root => {
         open Json;
         let newRoot = root |> Map.add("asdf", string("paulus"));
         switch (root |> Map.get("ufoo") |?> List.ofJson) {
         | Some(ufoo) =>
           newRoot
           |> Map.add("ufoo", ufoo |> List.set(2, int(15)) |> List.toJson)
         | None => newRoot
         };
       })
  );

let d2b =
  UniAM.(
    d1
    |> change("get dizzy", root => {
         open Json;
         let newRoot = root |> Map.add("asdf", string("omangus"));
         switch (root |> Map.get("ufoo") |?> List.ofJson) {
         | Some(ufoo) =>
           newRoot
           |> Map.add("ufoo", ufoo |> List.set(2, int(9)) |> List.toJson)
         | None => newRoot
         };
       })
  );

let merged = merge(d2a, d2b);

let clockForRemote = d2a |> UniAM.getClock;
let changesFromRemote = d2b |> UniAM.getChangesFromTime(clockForRemote);
let merged2 = d2a |> UniAM.applyChanges(changesFromRemote);

/* switch (merged |> UniAM.root |> UniAM.Json.Map.get("ufoo")) {
   | Some(ufoo) =>
     let conflicts = ufoo->UniAM.getArrayConflicts;
     Js.log(conflicts);
     ();
   | None => ()
   }; */
Js.log(clockForRemote);
Js.log("===========");
Js.log(merged2);

/* ====================================== */
/* Dream API                              */
/* ====================================== */

/* ------------- */
/* a) Imperative */
/* ------------- */

/* module AM = Automerge;
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
                k === "B" ?
                  switch (v) {
                  | Json.Array(items) =>
                    Json.Array(
                      List.fold_right(
                        (v, (i, acc)) => [i === 1 ? Json.Int(42) : v, ...acc],
                        items,
                        (0, []),
                      )
                      |> snd,
                    )
                  | _ => Json.Array([])
                  } :
                  v
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

   let schema = AM.Object([("me", true, AM.Object([()]))]); */

/* ====================================== */
/* AMPure playground                      */
/* ====================================== */

Js.log("===========");
let logState = s => {
  let reformat: string => string = [%bs.raw
    s => "return JSON.stringify(JSON.parse(s), null, 3);"
  ];
  Js.log(reformat(Json.stringify(AMPure.encodeState(s))));
};

let docA = AMPure.make("aaaaaaaa");
logState(docA);

let docA =
  AMPure.(
    docA
    |> makeAssign(doc, EmptyObject)
    |> makeAssign(doc->get("AAA"), Int(1111))
    |> makeAssign(doc->get("BBB"), Int(2222))
    |> makeDelete(doc->get("AAA"))
  );

let ops = docA |> AMPure.send;

let docB =
  AMPure.(
    make("bbbbbbbbb")
    |> makeAssign(doc, EmptyObject)
    |> makeAssign(doc->get("CCC"), EmptyArray)
  );
let docB =
  AMPure.(docB |> makeInsert(doc->get("CCC")->idx(0, docB), Int(42)));
let docB =
  AMPure.(
    docB
    |> makeAssign(doc->get("CCC")->idx(1, docB), Int(43))
    |> recv(ops)
  );

let ops =
  AMPure.(
    docA
    |> recv(docB |> send)
    |> makeAssign(doc->get("BBB"), Int(333))
    |> send
  );

let docB = AMPure.(docB |> makeAssign(doc->get("BBB"), Int(444)));

let docB = AMPure.(docB |> recv(ops));

logState(docB);