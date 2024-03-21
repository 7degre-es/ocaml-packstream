module Unary = struct
  module type S = sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module type LET = sig
    type 'a t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module type INFIX = sig
    type 'a t

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Make_let (Monad : S) : LET with type 'a t = 'a Monad.t = struct
    type 'a t = 'a Monad.t

    let ( let+ ) v f = Monad.map f v
    let ( let* ) = Monad.bind
  end

  module Make_infix (Monad : S) : INFIX with type 'a t = 'a Monad.t = struct
    type 'a t = 'a Monad.t

    let ( >|= ) v f = Monad.map f v
    let ( >>= ) = Monad.bind
  end
end

module Binary = struct
  module type S = sig
    type ('a, 'b) t

    val return : 'a -> ('a, _) t
    val bind : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
    val map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  end

  module type LET = sig
    type ('a, 'b) t

    val ( let+ ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
    val ( let* ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
  end

  module type INFIX = sig
    type ('a, 'b) t

    val ( >|= ) : ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
    val ( >>= ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
  end

  module Make_let (Monad : S) : LET with type ('a, 'b) t = ('a, 'b) Monad.t =
  struct
    type ('a, 'b) t = ('a, 'b) Monad.t

    let ( let+ ) v f = Monad.map f v
    let ( let* ) = Monad.bind
  end

  module Make_infix (Monad : S) :
    INFIX with type ('a, 'b) t = ('a, 'b) Monad.t = struct
    type ('a, 'b) t = ('a, 'b) Monad.t

    let ( >|= ) v f = Monad.map f v
    let ( >>= ) = Monad.bind
  end
end
