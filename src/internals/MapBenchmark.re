module StringMap = Map.Make(String);

let gen_passwd = {
  let alphanum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  let len = String.length(alphanum);
  fun
  | n => {
      let str = Bytes.create(n);
      for (i in 0 to pred(n)) {
        Bytes.set(str, i, alphanum.[Random.int(len)]);
      };
      Bytes.to_string(str);
    };
};

let createTestMap = count => {
  let rec createTestMapF =
    fun
    | (0, result) => result
    | (remaining, result) => {
        let key = gen_passwd(10);
        let value = gen_passwd(20);
        createTestMapF((remaining - 1, result |> StringMap.add(key, value)));
      };
  createTestMapF((count, StringMap.empty));
};

Js.log("started");
let x = createTestMap(100000) |> StringMap.add("repesyasdf", "asdf");
Js.log("created");
let y = x |> StringMap.add("repes", "asdf");
Js.log(x == y);
Js.log(x |> StringMap.find("repesyasdf"));