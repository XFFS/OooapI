open Lib.Http_spec

let show pp x = Format.printf "%a\n%!" pp x

let%expect_test "can construct locator params" =
  let open Resource.Param in
  let show (type a) (param : a t) (x : a) = Format.printf "%a\n%!" param.pp x in
  show (str "a str") "value";
  [%expect {|value|}];
  show (int "a int") 42;
  [%expect {|42|}];
  show (float "a float") 42.42;
  [%expect {|42.42|}];
  show (bool "a bool") false;
  [%expect {|false|}]

let%expect_test "can construct locator paths" =
  let open Resource in
  let path =
    Locator.Path.("foo" @/ "bar" @/ Param.str "name" @? Param.int "id" @? nil)
  in
  Format.printf "%a\n%!" Locator.Path.pp path;
  [%expect {| foo/bar/{name}/{id} |}]

let%expect_test "can construct locators" =
  let open Resource in
  let locator =
    let open Locator in
    "https://example.com"
    |> v
         ~path:
           Path.("foo" @/ "bar" @/ Param.str "name" @? Param.int "id" @? nil)
         ~query:Params.(Param.int "age" @/ Param.str "who" @/ nil)
    |> to_uri ~path:Args.("Bob" @/ 42 @/ nil) ~query:Args.(23 @/ "Alice" @/ nil)
  in
  Format.printf "%a\n%!" Uri.pp_hum locator;
  [%expect {| https://example.com/foo/bar/Bob/42?age=23&who=Alice |}]