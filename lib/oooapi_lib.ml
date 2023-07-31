open AstUtil

let endpoint =
  [%str
    module type Server = sig
      val uri : string
    end

    module type Config = sig
      val bearer_token : string option
      (** A token added as a bearer in the [Authorization] header*)

      val default_headers : Cohttp.Header.t option
      (** Headers to be appended to every request. *)
    end

    module Multipart : sig
      val part_of_file :
           name:string
        -> source:string
        -> filename:string
        -> Multipart_form.part Lwt.t

      val part_of_string : name:string -> content:string -> Multipart_form.part

      val form :
           ?header:Multipart_form.Header.t
        -> ?boundary:string
        -> Multipart_form.part list
        -> Multipart_form.multipart

      type part_data =
        [ `String of string * string  (** `String (name, content)*)
        | `File of string * string  (** `Label (name, source)*)
        ]
      (** The data needed to create a part of a multipart form *)

      val form_of_data : part_data list -> Multipart_form.multipart Lwt.t
    end = struct
      open Multipart_form

      let stream_of_file : string -> (string * int * int) Multipart_form.stream
          =
        let chunk_size = 0x1000 in
        fun filename ->
          let ic = In_channel.open_bin filename in
          let buf = Bytes.create chunk_size in
          fun () ->
            match In_channel.input ic buf 0 chunk_size with
            | exception exn ->
                In_channel.close ic;
                raise exn
            | 0 ->
                In_channel.close ic;
                None
            | len ->
                let str = Bytes.sub_string buf 0 len in
                Some (str, 0, len)

      let part_of_file ~name ~source ~filename =
        let disposition = Content_disposition.v name ~filename in
        Lwt.return (part ~disposition (stream_of_file source))

      let stream_of_string x =
        let once = ref false in
        let go () =
          if !once then
            None
          else (
            once := true;
            Some (x, 0, String.length x)
          )
        in
        go

      let part_of_string ~name ~content =
        let disposition = Content_disposition.v name in
        part ~disposition (stream_of_string content)

      let form ?header ?boundary parts =
        let rng ?g:_ _ = Uuidm.(v5 ns_X500 "boundary" |> to_string) in
        Multipart_form.multipart ~rng ?header ?boundary parts

      type part_data =
        [ `String of string * string  (** `String (name, content)*)
        | `File of string * string  (** `Label (name, source)*)
        ]
      (** The data needed to create a part of a multipart form *)

      let form_of_data : part_data list -> Multipart_form.multipart Lwt.t =
       fun part_data ->
        let open Lwt.Syntax in
        let+ parts =
          part_data
          |> Lwt_list.map_p (function
                 | `String (name, content) ->
                     Lwt.return (part_of_string ~name ~content)
                 | `File (name, source) ->
                     part_of_file
                       ~name
                       ~source
                       ~filename:
                         (String.split_on_char '/' source |> List.rev |> List.hd))
        in
        form parts
    end

    module EndpointLib (Server : Server) (Config : Config) = struct
      let base_uri = Server.uri

      let default_headers =
        let headers =
          match Config.default_headers with
          | None -> Cohttp.Header.init ()
          | Some hs -> hs
        in
        match Config.bearer_token with
        | None -> headers
        | Some token ->
            Cohttp.Header.add headers "Authorization" ("Bearer " ^ token)

      type request_err =
        [ `Request of Cohttp.Code.status_code * string
        | `Deseriaization of string * string
        ]

      type 'a request_result = ('a, request_err) result Lwt.t

      type data =
        [ `Json of Yojson.Safe.t
        | `Multipart_form of Multipart.part_data list
        ]

      let of_json_string f s = f (Yojson.Safe.from_string s)

      let make_request
          ?(data : data option)
          ~(path : string list)
          ~(params : (string * string) list)
          ~(headers : (string * string) list)
          ~(decode : string -> ('resp, string) result)
          (meth : Cohttp.Code.meth) =
        let open Lwt.Syntax in
        let* body, content_headers =
          match data with
          | None -> Lwt.return (None, [])
          | Some (`Json data_json) ->
              let data_str = Yojson.Safe.to_string data_json in
              let data_body = Cohttp_lwt.Body.of_string data_str in
              Lwt.return
                (Some data_body, [ ("Content-Type", "application/json") ])
          | Some (`Multipart_form data_parts) ->
              let* form = Multipart.form_of_data data_parts in
              let headers, data_body  =
                Multipart_form_cohttp.Client.multipart_form form
              in
              Lwt.return (Some data_body, Cohttp.Header.to_list headers)
        in
        let uri =
          let path_parts = base_uri :: path in
          let uri_str = String.concat "/" path_parts in
          let base_uri = Uri.of_string uri_str in
          Uri.add_query_params' base_uri params
        in
        let req_headers =
          Cohttp.Header.add_list default_headers (headers @ content_headers)
        in
        let* resp, resp_body =
          Cohttp_lwt_unix.Client.call ~headers:req_headers ?body meth uri
        in
        let+ resp_body_str = Cohttp_lwt.Body.to_string resp_body in
        match resp.status with
        | `OK -> (
            match decode resp_body_str with
            | Ok resp_data -> Ok resp_data
            | Error e -> Error (`Deseriaization (resp_body_str, e)))
        | other -> Error (`Request (other, resp_body_str))
    end]
