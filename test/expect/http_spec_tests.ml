open Lib.Http_spec

let show pp x = Format.printf "%a\n%!" pp x

let%expect_test "can construct locator params" =
  let open Resource.Locator.Param in
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
  let open Resource.Locator in
  let path =
    Path.("foo" @/ "bar" @/ Param.str "name" @? Param.int "id" @? nil)
  in
  Format.printf "%a\n%!" Path.pp path;
  [%expect {| foo/bar/{name}/{id} |}]

let%expect_test "can construct locators" =
  let open Resource.Locator in
  let path =
    Path.("foo" @/ "bar" @/ Param.str "name" @? Param.int "id" @? nil)
  in
  let args = Args.("Bob" @/ 42 @/ nil) in
  let locator = v "https://example.com" path |> to_uri args in
  Format.printf "%a\n%!" Uri.pp_hum locator;
  [%expect {| https://example.com/foo/bar/Bob/42 |}]
