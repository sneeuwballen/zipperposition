
module Z = struct
  include Z
  let to_int_exn = to_int
end
module Q = struct
  include Q
end
