module Doc = {
  [@bs.deriving abstract]
  type t = {
    _conflicts: Automerge.objectConflicts,
    mutable asdf: string,
    mutable ufoo: array(int),
  };
};

module DocA = Automerge.Make(Doc);

let d1 =
  DocA.(
    init(~actorId="aaaaaaaa", ())
    ->change("test", d => {
        d->Doc.ufooSet([|42, 20, 11, 19|]);
        d->Doc.asdfSet("asdf");
      })
  );

let d2a =
  DocA.(
    init(~actorId="bbbbbbbb", ())
    ->merge(d1)
    ->change("get crazy", d => {
        d->Doc.asdfSet("paulus");
        d->Doc.ufooGet[2] = 15;
      })
  );

let d2b =
  DocA.(
    d1->change("get dizzy", d => {
      d->Doc.asdfSet("omangus");
      d->Doc.ufooGet[2] = 9;
    })
  );

let merged = DocA.merge(d2a, d2b);

let clockForRemote = d2a->DocA.getClock;
let changesFromRemote = d2b->DocA.getMissingChanges(clockForRemote);
let merged2 = d2a->DocA.applyChanges(changesFromRemote);

let conflicts = merged->Doc.ufooGet->Automerge.getArrayConflicts;
Js.log(merged);
Js.log("===========");
Js.log(merged2);
/* Js.log(DocA.getClock(d1)); */

/* Js.log(
     {
       switch (conflicts->Js.Dict.get("asdf")) {
       | Some(conf) =>
         switch (Js.Dict.get(conf, "aaaaaaaa")) {
         | Some(valu) =>
           switch (Js.Json.classify(valu)) {
           | Js.Json.JSONString(str) => str
           | _ => "omg, not string..."
           }
         | None => "omg, no aaaaa"
         }
       | None => "omg, no asdf conflict"
       };
     },
   ); */

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

/* ====================================== */
/* Dream API */
/* ====================================== */

/* AM = Automerge */
/* type d = AM.init();
   d->AM.change("Something", d => {...d, a: "aval"}); */

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