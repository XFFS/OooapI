(** A simplified, normalized representation of HTTP API's *)

module O = Openapi_spec
module Ast = Ppxlib.Ast

(** A message

   > A client sends requests to a server in the form of a request message with a method (Section 9) and request target (Section 7.1). The request
   might also contain header fields (Section 6.3) for request modifiers, client information, and representation metadata, content (Section 6.4)
   intended for processing in accordance with the method, and trailer fields (Section 6.5) to communicate information collected while sending
   the content.¶
   >
   > A server responds to a client's request by sending one or more response messages, each including a status code (Section 15). The response
   might also contain header fields for server information, resource metadata, and representation metadata, content to be interpreted in
   accordance with the status code, and trailer fields to communicate information collected while sending the content.¶
*)
module Message = struct
  type to_data = [ `To of Ast.expression ]
  type of_data = [ `Of of Ast.expression ]

  type param =
    { name : string
    ; required : bool
    ; conv : to_data
    }

  (* Parameters that specialize a request *)
  type params =
    { path : param list
    ; query : param list
    ; header : param list
    ; cookie : param list
    }

  type request =
    { params : params
    ; path : O.Openapi_path.t (* Locator *)
    ; meth : Http.Method.t
    ; content_conv : to_data option (* Representation (if applicable) *)
    }

  (* TODO: Support different status returns? E.g. via [`200 (of_data data)] *)
  type responses = (Http.Status.t * of_data) list
  type t = request * responses
end

type t =
  { base_url : string
  ; messages : Message.t list
  }

let of_openapi_spec : Openapi_spec.t -> t = fun _ -> raise (Failure "TODO")
