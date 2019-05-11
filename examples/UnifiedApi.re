/** Demonstration of provided binding API that emulates more idiomatic functional usage (has no assignment in [change] function). It uses Automerge JS implementation under the hood right now, but the interface should be universal enough to stay the same even for a purely functional implementation.

THIS API IS RECOMMENDED for general usage.

*/;

open InfixRe;

module MActorId = {
  type t = string;
  let ofString = t => Some(t);
  let toString = t => t;
  let ofStringExn = t => t;
};

module UniAM = Automerge.UniJs.Make(MActorId);

let d1 =
  UniAM.(
    make(MActorId.ofStringExn("aaaaaaaa"))
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
    make(MActorId.ofStringExn("bbbbbbbb"))
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
Js.log2("merged2", merged2);

UniAM.(
  merged2
  |> root
  |> Json.Map.getC("asdf")
  |?>> (
    conflictable => {
      let conflictingValues =
        switch (conflictable.conflicts) {
        | Some(conflicts) =>
          Json.ConflictValues.fold(
            (_actorId, value, lst) => [value, ...lst],
            conflicts,
            [],
          )
        | None => []
        };
      Js.log(
        [conflictable.value, ...conflictingValues]
        ->Belt.List.keepMap(Json.asString),
      );
    }
  )
  |? ()
);