namespace Jellyfish.Core

[<RequireQualifiedAccess>]
module Option =    
    let combine = function | Some a, Some b -> Some (a, b) | _ -> None
