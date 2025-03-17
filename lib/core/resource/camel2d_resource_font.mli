type t

val load : string -> t Promise.promise
val font_family_of : t -> string