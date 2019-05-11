/** A demo for provided API that closely copies the usage experience of the original JavaScript library, including mutation inside [change] functions. */;

open InfixRe;

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
  Js.log2("ufoo conflicts:", conflicts);
  ();
| None => ()
};

let conflicts = merged->AM.getObjectConflicts;
Js.log2("asdf conflicts:", conflicts);

let merged2b =
  AM.(
    merged->change("get dizzy", d => {
      switch (d->Js.Dict.get("ufoo") |?> Js.Json.decodeArray) {
      | Some(ufoo) => ufoo[2] = Js.Json.number(9.0)
      | None => ()
      };
      d->Js.Dict.set("ufoo", Js.Json.string("peperoni"));
      d->Js.Dict.set("asdf", Js.Json.string("omangus"));
    })
  );
let conflicts = merged2b->AM.getObjectConflicts;
Js.log2("asdf conflicts:", conflicts);

Js.log2("merged", merged);
Js.log2("merged2", merged2);