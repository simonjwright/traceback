This project's purpose is to check out a proposed patch to GCC on as many architectures as possible (rather than just my M1 Macs), using a Github workflow.

When an unhandled exception occurs, the traceback report includes the load address, which is necessary in order to decode the error. On the other hand, the report output by GNAT.Traceback.Symbolic (a renaming of System.Traceback.Symbolic) doesn’t.

Without the patch, the different reports are (x86_64 macOS)

```
--- Ada.Exceptions.Exception_Information ---
raised CONSTRAINT_ERROR : traceback.adb:7 explicit raise
Load address: 0x1045e4000
Call stack traceback locations:
0x1045e5dcd 0x1045e5d41

--- GNAT.Traceback.Symbolic.Symbolic_Traceback ---
0x00000001045E5DCD 0x00000001045E5D41
```

and with the patch

```
--- Ada.Exceptions.Exception_Information ---
raised CONSTRAINT_ERROR : traceback.adb:7 explicit raise
Load address: 0x10ff88000
Call stack traceback locations:
0x10ff8ab0f 0x10ff8b267

--- GNAT.Traceback.Symbolic.Symbolic_Traceback ---
Load address: 0x10FF88000
0x10FF8AB0F 0x10FF8B267
```

From the Action logs, the equivalent results for Windows are

```
--- Ada.Exceptions.Exception_Information ---
raised CONSTRAINT_ERROR : traceback.adb:7 explicit raise
Load address: 0x7ff7c1110000
Call stack traceback locations:
0x7ff7c111161a 0x7ff7c1111db6 0x7ff7c111133e 0x7ff7c1111144 0x7ffae6fd4cae 0x7ffae8bbecd9

--- GNAT.Traceback.Symbolic.Symbolic_Traceback ---
Load address: 0x7FF7C1110000
0x7FF7C111161A 0x7FF7C1111DB6 0x7FF7C111133E 0x7FF7C1111144 0x7FFAE6FD4CAE 0x7FFAE8BBECD9
```

and for Ubuntu

```
--- Ada.Exceptions.Exception_Information ---
raised CONSTRAINT_ERROR : traceback.adb:7 explicit raise
Call stack traceback locations:
0x403040 0x4036b9 0x7fbce0c29d8e 0x7fbce0c29e3e 0x402f33 0xfffffffffffffffe

--- GNAT.Traceback.Symbolic.Symbolic_Traceback ---
0x403040 0x4036B9 0x7FBCE0C29D8E 0x7FBCE0C29E3E 0x402F33 0xFFFFFFFFFFFFFFFE
```
