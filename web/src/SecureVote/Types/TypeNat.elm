module SecureVote.Types.TypeNat exposing (OnePlus, Zero)

{-| Easy to remember names for type-level natural numbers.
These are intended only to be used as "phantom types:"
that is, there are no values of type `Zero` or `OnePlus a`.
Instead, they should be arguments to other type constructors.
Elm doesn't have DataKinds, so please don't do anything silly like
`OnePlus (OnePlus (OnePlus (List (Int, Int))))`
i.e. please only give another TypeNat as the argument to
OnePlus.
@docs Zero, OnePlus
-}

{-
    @author: https://github.com/JoeyEremondi/elm-typenats

   Copyright (c) 2015, Joey Eremondi
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

   * Neither the name of elm-typenats nor the names of its
     contributors may be used to endorse or promote products derived from
     this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}


{-| A phantom type for 0, our base TypeNat.
-}
type Zero
    = Zero Zero


{-| A phantom type constructor to increment a TypeNat,
creating a larger TypeNat.
-}
type OnePlus a
    = OnePlus OnePlus
