{-
This example is copied from https://copilot-language.github.io/

(c) 2009 Frank Dedden, Nis Nordby Wegmann, Lee Pike, Robin Morisset, Sebastian Niller, Alwyn Goodloe

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

Neither the name of the developers nor the names of its contributors
may be used to endorse or promote products derived from this software
without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

-- This is a simple example with basic usage. It implements a simple home
-- heating system: It heats when temp gets too low, and stops when it is high
-- enough. It read temperature as a byte (range -50C to 100C) and translates
-- this to Celcius.

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div)

-- External temperature as a byte, range of -50C to 100C
temp :: Stream Word8
temp = extern "temperature" Nothing

-- Calculate temperature in Celcius.
-- We need to cast the Word8 to a Float. Note that it is an unsafeCast, as there
-- is no direct relation between Word8 and Float.
ctemp :: Stream Float
ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0

spec = do
  -- Triggers that fire when the ctemp is too low or too high,
  -- pass the current ctemp as an argument.
  trigger "heaton"  (ctemp < 18.0) [arg ctemp]
  trigger "heatoff" (ctemp > 21.0) [arg ctemp]

-- Compile the spec
main = reify spec >>= compile "heater"
