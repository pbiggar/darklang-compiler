HostStructuralFormat adapts the breakable block layout algorithms from
dotnet/fsharp, src/Compiler/Utilities/sformat.fs, blob
e97978f46154b95fa87612bc655b54189cbd441d.

The MIT License (MIT)

Copyright (c) Microsoft Corporation.
All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

host_collation.c adapts the forward, case-sensitive SimpleAffix collation-
element algorithm from dotnet/runtime,
src/native/libs/System.Globalization.Native/pal_collation.c (release/11.0).
The source file used has SHA256 d2a12a800d4bf905ad61d47ace991d64438476161bb4f3f6017dfc37142c7c03.
This adapter links to ICU directly; it has no .NET runtime dependency.

Copyright (c) .NET Foundation and Contributors.
Licensed under the MIT license reproduced above.
