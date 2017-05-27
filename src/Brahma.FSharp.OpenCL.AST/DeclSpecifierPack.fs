// Copyright (c) 2017 Kirill Smirenko <k.smirenko@gmail.com>
// All rights reserved.
//
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.

namespace Brahma.FSharp.OpenCL.AST

type DeclSpecifierPack<'lang> (?funQual:FunQualifier<'lang>,
                               ?addrSpaceQual:AddressSpaceQualifier<'lang>,
                               ?accessQual:AccessQualifier<'lang>,
                               ?storClassSpec:StorageClassSpecifier<'lang>,
                               ?typeSpec:Type<'lang>,
                               ?typeQuals:TypeQualifier<'lang> list) =
    inherit Node<'lang>()
    override this.Children = []
    member val FunQual = funQual with get, set
    member val AddressSpaceQual = defaultArg addrSpaceQual Default with get, set
    member val AccessQual = accessQual with get, set
    member val StorageClassSpec = storClassSpec with get, set
    member val Type = typeSpec with get, set
    member val TypeQuals = defaultArg typeQuals [] with get, set

    member this.AddTypeQual tq =
        this.TypeQuals <- tq :: this.TypeQuals

    member this.Matches(other:obj) =
        match other with
        | :? DeclSpecifierPack<'lang> as o ->
            let areTypesMatching =
                match this.Type, o.Type with
                | Some x, Some y -> x.Matches(y)
                | None, None -> true
                | _ -> false
            this.FunQual = o.FunQual
            && this.AddressSpaceQual = o.AddressSpaceQual
            && this.AccessQual = o.AccessQual
            && this.StorageClassSpec = o.StorageClassSpec
            && areTypesMatching
            && this.TypeQuals = o.TypeQuals
        | _ -> false
