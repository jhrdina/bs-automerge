/** Demo that shows experimental purely functional implementation of the {{: https:/www.cl.cam.ac.uk/~arb33/papers/KleppmannBeresford-CRDT-JSON-TPDS2017.pdf} original paper}. It is much smaller than Automerge, however it lacks a lot of features and performance improvements Automerge added on top of the article. */

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