------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . O B J E C T _ R E A D E R                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2009-2024, Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Interfaces.C;

with System.CRTL;

with GNAT.IO;

package body System.Object_Reader is

   use Interfaces;
   use Interfaces.C;
   use System.Mmap;

   SSU : constant := System.Storage_Unit;

   function To_int32 is new Ada.Unchecked_Conversion (uint32, int32);

   function Trim_Trailing_Nuls (Str : String) return String;
   --  Return a copy of a string with any trailing NUL characters truncated

   procedure Check_Read_Offset (S : Mapped_Stream; Size : uint32);
   --  Check that the SIZE bytes at the current offset are still in the stream

   -------------------------------------
   -- ELF object file format handling --
   -------------------------------------

   generic
      type uword is mod <>;

   package ELF_Ops is

      --  ELF version codes

      ELFCLASS32 : constant := 1;  --  32 bit ELF
      ELFCLASS64 : constant := 2;  --  64 bit ELF

      --  ELF machine codes

      EM_NONE        : constant :=  0; --  No machine
      EM_SPARC       : constant :=  2; --  SUN SPARC
      EM_386         : constant :=  3; --  Intel 80386
      EM_MIPS        : constant :=  8; --  MIPS RS3000 Big-Endian
      EM_MIPS_RS3_LE : constant := 10; --  MIPS RS3000 Little-Endian
      EM_SPARC32PLUS : constant := 18; --  Sun SPARC 32+
      EM_PPC         : constant := 20; --  PowerPC
      EM_PPC64       : constant := 21; --  PowerPC 64-bit
      EM_ARM         : constant := 40; --  ARM
      EM_SPARCV9     : constant := 43; --  SPARC v9 64-bit
      EM_IA_64       : constant := 50; --  Intel Merced
      EM_X86_64      : constant := 62; --  AMD x86-64 architecture
      EM_AARCH64     : constant := 183; --  Aarch64

      EN_NIDENT  : constant := 16;

      type E_Ident_Type is array (0 .. EN_NIDENT - 1) of uint8;

      type Header is record
         E_Ident     : E_Ident_Type; -- Magic number and other info
         E_Type      : uint16;       -- Object file type
         E_Machine   : uint16;       -- Architecture
         E_Version   : uint32;       -- Object file version
         E_Entry     : uword;        -- Entry point virtual address
         E_Phoff     : uword;        -- Program header table file offset
         E_Shoff     : uword;        -- Section header table file offset
         E_Flags     : uint32;       -- Processor-specific flags
         E_Ehsize    : uint16;       -- ELF header size in bytes
         E_Phentsize : uint16;       -- Program header table entry size
         E_Phnum     : uint16;       -- Program header table entry count
         E_Shentsize : uint16;       -- Section header table entry size
         E_Shnum     : uint16;       -- Section header table entry count
         E_Shstrndx  : uint16;       -- Section header string table index
      end record;

      type Section_Header is record
         Sh_Name      : uint32; -- Section name string table index
         Sh_Type      : uint32; -- Section type
         Sh_Flags     : uword;  -- Section flags
         Sh_Addr      : uword;  -- Section virtual addr at execution
         Sh_Offset    : uword;  -- Section file offset
         Sh_Size      : uword;  -- Section size in bytes
         Sh_Link      : uint32; -- Link to another section
         Sh_Info      : uint32; -- Additional section information
         Sh_Addralign : uword;  -- Section alignment
         Sh_Entsize   : uword;  -- Entry size if section holds table
      end record;

      SHF_ALLOC : constant := 2;
      SHF_EXECINSTR : constant := 4;

      type Symtab_Entry32 is record
         St_Name  : uint32;  --  Name (string table index)
         St_Value : uint32;  --  Value
         St_Size  : uint32;  --  Size in bytes
         St_Info  : uint8;   --  Type and binding attributes
         St_Other : uint8;   --  Undefined
         St_Shndx : uint16;  --  Defining section
      end record;

      type Symtab_Entry64 is record
         St_Name  : uint32;  --  Name (string table index)
         St_Info  : uint8;   --  Type and binding attributes
         St_Other : uint8;   --  Undefined
         St_Shndx : uint16;  --  Defining section
         St_Value : uint64;  --  Value
         St_Size  : uint64;  --  Size in bytes
      end record;

      function Read_Header (F : in out Mapped_Stream) return Header;
      --  Read a header from an ELF format object

      function First_Symbol
        (Obj : in out ELF_Object_File) return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Read_Symbol
        (Obj : in out ELF_Object_File;
         Off : Offset) return Object_Symbol;
      --  Read a symbol at offset Off

      function Name
        (Obj : in out ELF_Object_File;
         Sym : Object_Symbol) return String_Ptr_Len;
      --  Return the name of the symbol

      function Name
        (Obj : in out ELF_Object_File;
         Sec : Object_Section) return String;
      --  Return the name of a section

      function Get_Section
        (Obj   : in out ELF_Object_File;
         Shnum : uint32) return Object_Section;
      --  Fetch a section by index from zero

      function Initialize
        (F            : Mapped_File;
         Hdr          : Header;
         In_Exception : Boolean) return ELF_Object_File;
      --  Initialize an object file

   end ELF_Ops;

   -----------------------------------
   -- PECOFF object format handling --
   -----------------------------------

   package PECOFF_Ops is

      --  Constants and data layout are taken from the document "Microsoft
      --  Portable Executable and Common Object File Format Specification"
      --  Revision 8.1.

      Signature_Loc_Offset : constant := 16#3C#;
      --  Offset of pointer to the file signature

      Size_Of_Standard_Header_Fields : constant := 16#18#;
      --  Length in bytes of the standard header record

      Function_Symbol_Type : constant := 16#20#;
      --  Type field value indicating a symbol refers to a function

      Not_Function_Symbol_Type : constant := 16#00#;
      --  Type field value indicating a symbol does not refer to a function

      type Magic_Array is array (0 .. 3) of uint8;
      --  Array of magic numbers from the header

      --  Magic numbers for PECOFF variants

      VARIANT_PE32      : constant := 16#010B#;
      VARIANT_PE32_PLUS : constant := 16#020B#;

      --  PECOFF machine codes

      IMAGE_FILE_MACHINE_I386  : constant := 16#014C#;
      IMAGE_FILE_MACHINE_IA64  : constant := 16#0200#;
      IMAGE_FILE_MACHINE_AMD64 : constant := 16#8664#;

      --  PECOFF Data layout

      type Header is record
         Magics               : Magic_Array;
         Machine              : uint16;
         NumberOfSections     : uint16;
         TimeDateStamp        : uint32;
         PointerToSymbolTable : uint32;
         NumberOfSymbols      : uint32;
         SizeOfOptionalHeader : uint16;
         Characteristics      : uint16;
         Variant              : uint16;
      end record;
      pragma Pack (Header);

      type Optional_Header_PE32 is record
         Magic                       : uint16;
         MajorLinkerVersion          : uint8;
         MinorLinkerVersion          : uint8;
         SizeOfCode                  : uint32;
         SizeOfInitializedData       : uint32;
         SizeOfUninitializedData     : uint32;
         AddressOfEntryPoint         : uint32;
         BaseOfCode                  : uint32;
         BaseOfData                  : uint32; --  Note: not in PE32+
         ImageBase                   : uint32;
         SectionAlignment            : uint32;
         FileAlignment               : uint32;
         MajorOperatingSystemVersion : uint16;
         MinorOperationSystemVersion : uint16;
         MajorImageVersion           : uint16;
         MinorImageVersion           : uint16;
         MajorSubsystemVersion       : uint16;
         MinorSubsystemVersion       : uint16;
         Win32VersionValue           : uint32;
         SizeOfImage                 : uint32;
         SizeOfHeaders               : uint32;
         Checksum                    : uint32;
         Subsystem                   : uint16;
         DllCharacteristics          : uint16;
         SizeOfStackReserve          : uint32;
         SizeOfStackCommit           : uint32;
         SizeOfHeapReserve           : uint32;
         SizeOfHeapCommit            : uint32;
         LoaderFlags                 : uint32;
         NumberOfRvaAndSizes         : uint32;
      end record;
      pragma Pack (Optional_Header_PE32);
      pragma Assert (Optional_Header_PE32'Size = 96 * SSU);

      type Optional_Header_PE64 is record
         Magic                       : uint16;
         MajorLinkerVersion          : uint8;
         MinorLinkerVersion          : uint8;
         SizeOfCode                  : uint32;
         SizeOfInitializedData       : uint32;
         SizeOfUninitializedData     : uint32;
         AddressOfEntryPoint         : uint32;
         BaseOfCode                  : uint32;
         ImageBase                   : uint64;
         SectionAlignment            : uint32;
         FileAlignment               : uint32;
         MajorOperatingSystemVersion : uint16;
         MinorOperationSystemVersion : uint16;
         MajorImageVersion           : uint16;
         MinorImageVersion           : uint16;
         MajorSubsystemVersion       : uint16;
         MinorSubsystemVersion       : uint16;
         Win32VersionValue           : uint32;
         SizeOfImage                 : uint32;
         SizeOfHeaders               : uint32;
         Checksum                    : uint32;
         Subsystem                   : uint16;
         DllCharacteristics          : uint16;
         SizeOfStackReserve          : uint64;
         SizeOfStackCommit           : uint64;
         SizeOfHeapReserve           : uint64;
         SizeOfHeapCommit            : uint64;
         LoaderFlags                 : uint32;
         NumberOfRvaAndSizes         : uint32;
      end record;
      pragma Pack (Optional_Header_PE64);
      pragma Assert (Optional_Header_PE64'Size = 112 * SSU);

      subtype Name_Str is String (1 .. 8);

      type Section_Header is record
         Name                 : Name_Str;
         VirtualSize          : uint32;
         VirtualAddress       : uint32;
         SizeOfRawData        : uint32;
         PointerToRawData     : uint32;
         PointerToRelocations : uint32;
         PointerToLinenumbers : uint32;
         NumberOfRelocations  : uint16;
         NumberOfLinenumbers  : uint16;
         Characteristics      : uint32;
      end record;
      pragma Pack (Section_Header);

      IMAGE_SCN_CNT_CODE : constant := 16#0020#;

      type Symtab_Entry is record
         Name                  : Name_Str;
         Value                 : uint32;
         SectionNumber         : int16;
         TypeField             : uint16;
         StorageClass          : uint8;
         NumberOfAuxSymbols    : uint8;
      end record;
      pragma Pack (Symtab_Entry);

      type Auxent_Section is record
         Length              : uint32;
         NumberOfRelocations : uint16;
         NumberOfLinenumbers : uint16;
         CheckSum            : uint32;
         Number              : uint16;
         Selection           : uint8;
         Unused1             : uint8;
         Unused2             : uint8;
         Unused3             : uint8;
      end record;

      for Auxent_Section'Size use 18 * 8;

      function Read_Header (F : in out Mapped_Stream) return Header;
      --  Read the object file header

      function First_Symbol
        (Obj : in out PECOFF_Object_File) return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Read_Symbol
        (Obj : in out PECOFF_Object_File;
         Off : Offset) return Object_Symbol;
      --  Read a symbol at offset Off

      function Name
        (Obj : in out PECOFF_Object_File;
         Sym : Object_Symbol) return String_Ptr_Len;
      --  Return the name of the symbol

      function Name
        (Obj : in out PECOFF_Object_File;
         Sec : Object_Section) return String;
      --  Return the name of a section

      function Get_Section
        (Obj   : in out PECOFF_Object_File;
         Index : uint32) return Object_Section;
      --  Fetch a section by index from zero

      function Initialize
        (F            : Mapped_File;
         Hdr          : Header;
         In_Exception : Boolean) return PECOFF_Object_File;
      --  Initialize an object file

   end PECOFF_Ops;

   -------------------------------------
   -- XCOFF-32 object format handling --
   -------------------------------------

   package XCOFF32_Ops is

      --  XCOFF Data layout

      type Header is record
         f_magic  : uint16;
         f_nscns  : uint16;
         f_timdat : uint32;
         f_symptr : uint32;
         f_nsyms  : uint32;
         f_opthdr : uint16;
         f_flags  : uint16;
      end record;

      type Auxiliary_Header is record
         o_mflag      : uint16;
         o_vstamp     : uint16;
         o_tsize      : uint32;
         o_dsize      : uint32;
         o_bsize      : uint32;
         o_entry      : uint32;
         o_text_start : uint32;
         o_data_start : uint32;
         o_toc        : uint32;
         o_snentry    : uint16;
         o_sntext     : uint16;
         o_sndata     : uint16;
         o_sntoc      : uint16;
         o_snloader   : uint16;
         o_snbss      : uint16;
         o_algntext   : uint16;
         o_algndata   : uint16;
         o_modtype    : uint16;
         o_cpuflag    : uint8;
         o_cputype    : uint8;
         o_maxstack   : uint32;
         o_maxdata    : uint32;
         o_debugger   : uint32;
         o_flags      : uint8;
         o_sntdata    : uint16;
         o_sntbss     : uint16;
      end record;
      pragma Unreferenced (Auxiliary_Header);
      --  Not used, but not removed (just in case)

      subtype Name_Str is String (1 .. 8);

      type Section_Header is record
         s_name    : Name_Str;
         s_paddr   : uint32;
         s_vaddr   : uint32;
         s_size    : uint32;
         s_scnptr  : uint32;
         s_relptr  : uint32;
         s_lnnoptr : uint32;
         s_nreloc  : uint16;
         s_nlnno   : uint16;
         s_flags   : uint32;
      end record;
      pragma Pack (Section_Header);

      STYP_TEXT : constant := 16#0020#;

      type Symbol_Entry is record
         n_name   : Name_Str;
         n_value  : uint32;
         n_scnum  : uint16;
         n_type   : uint16;
         n_sclass : uint8;
         n_numaux : uint8;
      end record;
      for Symbol_Entry'Size use 18 * 8;

      type Aux_Entry is record
         x_scnlen   : uint32;
         x_parmhash : uint32;
         x_snhash   : uint16;
         x_smtyp    : uint8;
         x_smclass  : uint8;
         x_stab     : uint32;
         x_snstab   : uint16;
      end record;
      for Aux_Entry'Size use 18 * 8;
      pragma Pack (Aux_Entry);

      C_EXT     : constant := 2;
      C_HIDEXT  : constant := 107;
      C_WEAKEXT : constant := 111;

      XTY_LD : constant := 2;
      --  Magic constant should be documented, especially since it's changed???

      function Read_Header (F : in out Mapped_Stream) return Header;
      --  Read the object file header

      function First_Symbol
        (Obj : in out XCOFF32_Object_File) return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Read_Symbol
        (Obj : in out XCOFF32_Object_File;
         Off : Offset) return Object_Symbol;
      --  Read a symbol at offset Off

      function Name
        (Obj : in out XCOFF32_Object_File;
         Sym : Object_Symbol) return String_Ptr_Len;
      --  Return the name of the symbol

      function Name
        (Obj : in out XCOFF32_Object_File;
         Sec : Object_Section) return String;
      --  Return the name of a section

      function Initialize
        (F            : Mapped_File;
         Hdr          : Header;
         In_Exception : Boolean) return XCOFF32_Object_File;
      --  Initialize an object file

      function Get_Section
          (Obj   : in out XCOFF32_Object_File;
           Index : uint32) return Object_Section;
      --  Fetch a section by index from zero

   end XCOFF32_Ops;

   -----------------------------------
   -- MACH_O object format handling --
   -----------------------------------

   package MACH_O_Ops is

      pragma Warnings (Off, "is not referenced");

      MH_MAGIC_64 : constant := 16#feed_facf#;
      MH_EXECUTE  : constant := 16#2#;

      type mach_header_64 is record
         magic      : aliased uint32;
         cputype    : aliased int32;
         cpusubtype : aliased int32;
         filetype   : aliased uint32;  -- executable if MH_EXECUTE.
         ncmds      : aliased uint32;
         sizeofcmds : aliased uint32;
         flags      : aliased uint32;
         reserved   : aliased uint32;
      end record with
        Convention => C_Pass_By_Copy;

      subtype Header is mach_header_64;

      function Read_Header (F : in out Mapped_Stream) return Header;
      --  Read the object file header

      function First_Symbol
        (Obj : in out MACH_O_Object_File) return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Read_Symbol
        (Obj : in out MACH_O_Object_File;
         Off :        Offset) return Object_Symbol;
      --  Read a symbol at offset Off

      function Name
        (Obj : in out MACH_O_Object_File;
         Sym :        Object_Symbol) return String_Ptr_Len;
      --  Return the name of the symbol

      function Name
        (Obj : in out MACH_O_Object_File;
         Sec :        Object_Section) return String;
      --  Return the name of a section

      function Initialize
        (F            : Mapped_File;
         Hdr          : Header;
         In_Exception : Boolean) return MACH_O_Object_File;
      --  Initialize an object file

      function Get_Section
        (Obj   : in out MACH_O_Object_File;
         Index :        uint32) return Object_Section;
   --  Fetch a section by index from zero

      pragma Warnings (On, "is not referenced");

   end MACH_O_Ops;

   -------------
   -- ELF_Ops --
   -------------

   package body ELF_Ops is

      function Get_String_Table (Obj : in out ELF_Object_File)
                                return Object_Section;
      --  Fetch the section containing the string table

      function Get_Symbol_Table (Obj : in out ELF_Object_File)
                                return Object_Section;
      --  Fetch the section containing the symbol table

      function Read_Section_Header
        (Obj   : in out ELF_Object_File;
         Shnum : uint32) return Section_Header;
      --  Read the header for an ELF format object section indexed from zero

      ------------------
      -- First_Symbol --
      ------------------

      function First_Symbol
        (Obj : in out ELF_Object_File) return Object_Symbol
      is
      begin
         if Obj.Symtab_Last = 0 then
            return Null_Symbol;
         else
            return Read_Symbol (Obj, 0);
         end if;
      end First_Symbol;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj   : in out ELF_Object_File;
         Shnum : uint32) return Object_Section
      is
         SHdr : constant Section_Header := Read_Section_Header (Obj, Shnum);

      begin
         return (Shnum,
                 Offset (SHdr.Sh_Offset),
                 uint64 (SHdr.Sh_Addr),
                 uint64 (SHdr.Sh_Size),
                 (SHdr.Sh_Flags and SHF_EXECINSTR) /= 0);
      end Get_Section;

      ------------------------
      --  Get_String_Table  --
      ------------------------

      function Get_String_Table
        (Obj : in out ELF_Object_File) return Object_Section
      is
      begin
         --  All cases except MIPS IRIX, string table located in .strtab

         if Obj.Arch /= MIPS then
            return Get_Section (Obj, ".strtab");

         --  On IRIX only .dynstr is available

         else
            return Get_Section (Obj, ".dynstr");
         end if;
      end Get_String_Table;

      ------------------------
      --  Get_Symbol_Table  --
      ------------------------

      function Get_Symbol_Table
        (Obj : in out ELF_Object_File) return Object_Section
      is
      begin
         --  All cases except MIPS IRIX, symbol table located in .symtab

         if Obj.Arch /= MIPS then
            return Get_Section (Obj, ".symtab");

         --  On IRIX, symbol table located somewhere other than .symtab

         else
            return Get_Section (Obj, ".dynsym");
         end if;
      end Get_Symbol_Table;

      ----------------
      -- Initialize --
      ----------------

      function Initialize
        (F            : Mapped_File;
         Hdr          : Header;
         In_Exception : Boolean) return ELF_Object_File
      is
         Res : ELF_Object_File
           (Format => (case uword'Size is
                         when 64 => ELF64,
                         when 32 => ELF32,
                         when others => raise Program_Error));
         Sec : Object_Section;
      begin
         Res.MF := F;
         Res.In_Exception := In_Exception;
         Res.Num_Sections := uint32 (Hdr.E_Shnum);

         case Hdr.E_Machine is
            when EM_SPARC
               | EM_SPARC32PLUS
            =>
               Res.Arch := SPARC;

            when EM_386 =>
               Res.Arch := i386;

            when EM_MIPS
               | EM_MIPS_RS3_LE
            =>
               Res.Arch := MIPS;

            when EM_PPC =>
               Res.Arch := PPC;

            when EM_PPC64 =>
               Res.Arch := PPC64;

            when EM_SPARCV9 =>
               Res.Arch := SPARC64;

            when EM_IA_64 =>
               Res.Arch := IA64;

            when EM_X86_64 =>
               Res.Arch := x86_64;

            when EM_ARM =>
               Res.Arch := ARM;

            when EM_AARCH64 =>
               Res.Arch := AARCH64;

            when others =>
               raise Format_Error with "unrecognized architecture";
         end case;

         --  Map section table and section string table
         Res.Sectab_Stream := Create_Stream
           (F, File_Size (Hdr.E_Shoff),
            File_Size (Hdr.E_Shnum) * File_Size (Hdr.E_Shentsize));
         Sec := Get_Section (Res, uint32 (Hdr.E_Shstrndx));
         Res.Secstr_Stream := Create_Stream (Res, Sec);

         --  Map symbol and string table
         Sec := Get_Symbol_Table (Res);
         Res.Symtab_Stream := Create_Stream (Res, Sec);
         Res.Symtab_Last := Offset (Sec.Size);

         Sec := Get_String_Table (Res);
         Res.Symstr_Stream := Create_Stream (Res, Sec);

         return Res;
      end Initialize;

      -----------------
      -- Read_Header --
      -----------------

      function Read_Header (F : in out Mapped_Stream) return Header is
         Hdr : Header;

      begin
         Seek (F, 0);
         Read_Raw (F, Hdr'Address, uint32 (Hdr'Size / SSU));
         return Hdr;
      end Read_Header;

      -------------------------
      -- Read_Section_Header --
      -------------------------

      function Read_Section_Header
        (Obj   : in out ELF_Object_File;
         Shnum : uint32) return Section_Header
      is
         Shdr : Section_Header;

      begin
         Seek (Obj.Sectab_Stream, Offset (Shnum * Section_Header'Size / SSU));
         Read_Raw (Obj.Sectab_Stream, Shdr'Address, Section_Header'Size / SSU);
         return Shdr;
      end Read_Section_Header;

      -----------------
      -- Read_Symbol --
      -----------------

      function Read_Symbol
        (Obj : in out ELF_Object_File;
         Off : Offset) return Object_Symbol
      is
         ST_Entry32 : Symtab_Entry32;
         ST_Entry64 : Symtab_Entry64;
         Res        : Object_Symbol;

      begin
         Seek (Obj.Symtab_Stream, Off);

         case uword'Size is
            when 32 =>
               Read_Raw (Obj.Symtab_Stream, ST_Entry32'Address,
                         uint32 (ST_Entry32'Size / SSU));
               Res := (Off,
                       Off + ST_Entry32'Size / SSU,
                       uint64 (ST_Entry32.St_Value),
                       uint64 (ST_Entry32.St_Size));

            when 64 =>
               Read_Raw (Obj.Symtab_Stream, ST_Entry64'Address,
                         uint32 (ST_Entry64'Size / SSU));
               Res := (Off,
                       Off + ST_Entry64'Size / SSU,
                       ST_Entry64.St_Value,
                       ST_Entry64.St_Size);

            when others =>
               raise Program_Error;
         end case;

         return Res;
      end Read_Symbol;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : in out ELF_Object_File;
         Sec : Object_Section) return String
      is
         SHdr : Section_Header;

      begin
         SHdr := Read_Section_Header (Obj, Sec.Num);
         return Offset_To_String (Obj.Secstr_Stream, Offset (SHdr.Sh_Name));
      end Name;

      function Name
        (Obj : in out ELF_Object_File;
         Sym : Object_Symbol) return String_Ptr_Len
      is
         ST_Entry32 : Symtab_Entry32;
         ST_Entry64 : Symtab_Entry64;
         Name_Off   : Offset;

      begin
         --  Test that this symbol is not null

         if Sym = Null_Symbol then
            return (null, 0);
         end if;

         --  Read the symbol table entry

         Seek (Obj.Symtab_Stream, Sym.Off);

         case uword'Size is
            when 32 =>
               Read_Raw (Obj.Symtab_Stream, ST_Entry32'Address,
                         uint32 (ST_Entry32'Size / SSU));
               Name_Off := Offset (ST_Entry32.St_Name);

            when 64 =>
               Read_Raw (Obj.Symtab_Stream, ST_Entry64'Address,
                         uint32 (ST_Entry64'Size / SSU));
               Name_Off := Offset (ST_Entry64.St_Name);

            when others =>
               raise Program_Error;
         end case;

         --  Fetch the name from the string table

         Seek (Obj.Symstr_Stream, Name_Off);
         return Read (Obj.Symstr_Stream);
      end Name;

   end ELF_Ops;

   package ELF32_Ops is new ELF_Ops (uint32);
   package ELF64_Ops is new ELF_Ops (uint64);

   ----------------
   -- PECOFF_Ops --
   ----------------

   package body PECOFF_Ops is

      function Decode_Name
        (Obj      : in out PECOFF_Object_File;
         Raw_Name : String) return String;
      --  A section name is an 8 byte field padded on the right with null
      --  characters, or a '\' followed by an ASCII decimal string indicating
      --  an offset in to the string table. This routine decodes this

      function Get_Section_Virtual_Address
        (Obj   : in out PECOFF_Object_File;
         Index : uint32) return uint64;
      --  Fetch the address at which a section is loaded

      function Read_Section_Header
        (Obj   : in out PECOFF_Object_File;
         Index : uint32) return Section_Header;
      --  Read a header from section table

      function String_Table
        (Obj   : in out PECOFF_Object_File;
         Index : Offset) return String;
      --  Return an entry from the string table

      -----------------
      -- Decode_Name --
      -----------------

      function Decode_Name
        (Obj      : in out PECOFF_Object_File;
         Raw_Name : String) return String
      is
         Name_Or_Ref : constant String := Trim_Trailing_Nuls (Raw_Name);
         Off         : Offset;

      begin
         --  We should never find a symbol with a zero length name. If we do it
         --  probably means we are not parsing the symbol table correctly. If
         --  this happens we raise a fatal error.

         if Name_Or_Ref'Length = 0 then
            raise Format_Error with
              "found zero length symbol in symbol table";
         end if;

         if Name_Or_Ref (1) /= '/' then
            return Name_Or_Ref;
         else
            Off := Offset'Value (Name_Or_Ref (2 .. Name_Or_Ref'Last));
            return String_Table (Obj, Off);
         end if;
      end Decode_Name;

      ------------------
      -- First_Symbol --
      ------------------

      function First_Symbol
        (Obj : in out PECOFF_Object_File) return Object_Symbol
      is
      begin
         --  Return Null_Symbol in the case that the symbol table is empty

         if Obj.Symtab_Last = 0 then
            return Null_Symbol;
         end if;

         return Read_Symbol (Obj, 0);
      end First_Symbol;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj   : in out PECOFF_Object_File;
         Index : uint32) return Object_Section
      is
         Sec : constant Section_Header := Read_Section_Header (Obj, Index);

      begin
         --  Use VirtualSize instead of SizeOfRawData. The latter is rounded to
         --  the page size, so it may add garbage to the content. On the other
         --  side, the former may be larger than the latter in case of 0
         --  padding.

         return (Index,
                 Offset (Sec.PointerToRawData),
                 uint64 (Sec.VirtualAddress) + Obj.ImageBase,
                 uint64 (Sec.VirtualSize),
                 (Sec.Characteristics and IMAGE_SCN_CNT_CODE) /= 0);
      end Get_Section;

      ---------------------------------
      -- Get_Section_Virtual_Address --
      ---------------------------------

      function Get_Section_Virtual_Address
        (Obj   : in out PECOFF_Object_File;
         Index : uint32) return uint64
      is
         Sec : Section_Header;

      begin
         --  Try cache

         if Index = Obj.GSVA_Sec then
            return Obj.GSVA_Addr;
         end if;

         Obj.GSVA_Sec := Index;
         Sec := Read_Section_Header (Obj, Index);
         Obj.GSVA_Addr := Obj.ImageBase + uint64 (Sec.VirtualAddress);
         return Obj.GSVA_Addr;
      end Get_Section_Virtual_Address;

      ----------------
      -- Initialize --
      ----------------

      function Initialize
        (F            : Mapped_File;
         Hdr          : Header;
         In_Exception : Boolean) return PECOFF_Object_File
      is
         Res        : PECOFF_Object_File
           (Format => (case Hdr.Variant is
                         when PECOFF_Ops.VARIANT_PE32 => PECOFF,
                         when PECOFF_Ops.VARIANT_PE32_PLUS => PECOFF_PLUS,
                         when others => raise Program_Error
                                          with "unrecognized PECOFF variant"));
         Symtab_Size : constant Offset :=
           Offset (Hdr.NumberOfSymbols) * (Symtab_Entry'Size / SSU);
         Strtab_Size : uint32;
         Hdr_Offset : Offset;
         Opt_Offset : File_Size;
         Opt_Stream : Mapped_Stream;

      begin
         Res.MF := F;
         Res.In_Exception := In_Exception;

         case Hdr.Machine is
            when PECOFF_Ops.IMAGE_FILE_MACHINE_I386  =>
               Res.Arch := i386;
            when PECOFF_Ops.IMAGE_FILE_MACHINE_IA64  =>
               Res.Arch := IA64;
            when PECOFF_Ops.IMAGE_FILE_MACHINE_AMD64 =>
               Res.Arch := x86_64;
            when others =>
               raise Format_Error with "unrecognized architecture";
         end case;

         Res.Num_Sections := uint32 (Hdr.NumberOfSections);

         --  Map symbol table and the first following word (which is the length
         --  of the string table).

         Res.Symtab_Last  := Symtab_Size;
         Res.Symtab_Stream := Create_Stream
           (F,
            File_Size (Hdr.PointerToSymbolTable),
            File_Size (Symtab_Size + 4));

         --  Map string table. The first 4 bytes are the length of the string
         --  table and are part of it.

         Seek (Res.Symtab_Stream, Symtab_Size);
         Strtab_Size := Read (Res.Symtab_Stream);
         Res.Symstr_Stream := Create_Stream
           (F,
            File_Size (Hdr.PointerToSymbolTable) + File_Size (Symtab_Size),
            File_Size (Strtab_Size));

         --  Map section table

         Opt_Stream := Create_Stream (Res.MF, Signature_Loc_Offset, 4);
         Hdr_Offset := Offset (uint32'(Read (Opt_Stream)));
         Close (Opt_Stream);
         Res.Sectab_Stream := Create_Stream
           (F,
            File_Size (Hdr_Offset +
                         Size_Of_Standard_Header_Fields +
                         Offset (Hdr.SizeOfOptionalHeader)),
            File_Size (Res.Num_Sections)
              * File_Size (Section_Header'Size / SSU));

         --  Read optional header and extract image base

         Opt_Offset := File_Size (Hdr_Offset + Size_Of_Standard_Header_Fields);

         if Res.Format = PECOFF then
            declare
               Opt_32 : Optional_Header_PE32;
            begin
               Opt_Stream := Create_Stream
                 (Res.MF, Opt_Offset, Opt_32'Size / SSU);
               Read_Raw
                 (Opt_Stream, Opt_32'Address, uint32 (Opt_32'Size / SSU));
               Res.ImageBase := uint64 (Opt_32.ImageBase);
               Close (Opt_Stream);
            end;

         else
            declare
               Opt_64 : Optional_Header_PE64;
            begin
               Opt_Stream := Create_Stream
                 (Res.MF, Opt_Offset, Opt_64'Size / SSU);
               Read_Raw
                 (Opt_Stream, Opt_64'Address, uint32 (Opt_64'Size / SSU));
               Res.ImageBase := Opt_64.ImageBase;
               Close (Opt_Stream);
            end;
         end if;

         return Res;
      end Initialize;

      -----------------
      -- Read_Symbol --
      -----------------

      function Read_Symbol
        (Obj : in out PECOFF_Object_File;
         Off : Offset) return Object_Symbol
      is
         ST_Entry  : Symtab_Entry;
         ST_Last   : Symtab_Entry;
         Aux_Entry : Auxent_Section;
         Sz        : constant Offset := ST_Entry'Size / SSU;
         Result    : Object_Symbol;
         Noff      : Offset;
         Sym_Off   : Offset;

      begin
         --  Seek to the successor of Prev

         Noff := Off;

         loop
            Sym_Off := Noff;

            Seek (Obj.Symtab_Stream, Sym_Off);
            Read_Raw (Obj.Symtab_Stream, ST_Entry'Address, uint32 (Sz));

            --  Skip AUX entries

            Noff := Noff + Offset (1 + ST_Entry.NumberOfAuxSymbols) * Sz;

            exit when ST_Entry.TypeField = Function_Symbol_Type
              and then ST_Entry.SectionNumber > 0;

            if Noff >= Obj.Symtab_Last then
               return Null_Symbol;
            end if;
         end loop;

         --  Construct the symbol

         Result :=
           (Off   => Sym_Off,
            Next  => Noff,
            Value => uint64 (ST_Entry.Value),
            Size  => 0);

         --  Set the size as accurately as possible

         --  The size of a symbol is not directly available so we try scanning
         --  to the next function and assuming the code ends there.

         loop
            --  Read symbol and AUX entries

            Sym_Off := Noff;
            Seek (Obj.Symtab_Stream, Sym_Off);
            Read_Raw (Obj.Symtab_Stream, ST_Last'Address, uint32 (Sz));

            for I in 1 .. ST_Last.NumberOfAuxSymbols loop
               Read_Raw (Obj.Symtab_Stream, Aux_Entry'Address, uint32 (Sz));
            end loop;

            Noff := Noff + Offset (1 + ST_Last.NumberOfAuxSymbols) * Sz;

            if ST_Last.TypeField = Function_Symbol_Type then
               if ST_Last.SectionNumber = ST_Entry.SectionNumber
                 and then ST_Last.Value >= ST_Entry.Value
               then
                  --  Symbol is a function past ST_Entry

                  Result.Size := uint64 (ST_Last.Value - ST_Entry.Value);

               else
                  --  Not correlated function

                  Result.Next := Sym_Off;
               end if;

               exit;

            elsif ST_Last.SectionNumber = ST_Entry.SectionNumber
              and then ST_Last.TypeField = Not_Function_Symbol_Type
              and then ST_Last.StorageClass = 3
              and then ST_Last.NumberOfAuxSymbols = 1
            then
               --  Symbol is a section

               Result.Size := uint64 (ST_Last.Value + Aux_Entry.Length
                                        - ST_Entry.Value);
               Result.Next := Noff;
               exit;
            end if;

            exit when Noff >= Obj.Symtab_Last;
         end loop;

         --  Relocate the address

         Result.Value :=
           Result.Value + Get_Section_Virtual_Address
                            (Obj, uint32 (ST_Entry.SectionNumber - 1));

         return Result;
      end Read_Symbol;

      ------------------
      -- Read_Header  --
      ------------------

      function Read_Header (F : in out Mapped_Stream) return Header is
         Hdr : Header;
         Off : int32;

      begin
         --  Skip the MSDOS stub, and seek directly to the file offset

         Seek (F, Signature_Loc_Offset);
         Off := Read (F);

         --  Read the COFF file header

         Seek (F, Offset (Off));
         Read_Raw (F, Hdr'Address, uint32 (Hdr'Size / SSU));
         return Hdr;
      end Read_Header;

      -------------------------
      -- Read_Section_Header --
      -------------------------

      function Read_Section_Header
        (Obj   : in out PECOFF_Object_File;
         Index : uint32) return Section_Header
      is
         Sec : Section_Header;
      begin
         Seek (Obj.Sectab_Stream, Offset (Index * Section_Header'Size / SSU));
         Read_Raw (Obj.Sectab_Stream, Sec'Address, Section_Header'Size / SSU);
         return Sec;
      end Read_Section_Header;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : in out PECOFF_Object_File;
         Sec : Object_Section) return String
      is
         Shdr : constant Section_Header := Read_Section_Header (Obj, Sec.Num);
      begin
         return Decode_Name (Obj, Shdr.Name);
      end Name;

      -------------------
      -- String_Table  --
      -------------------

      function String_Table
        (Obj   : in out PECOFF_Object_File;
         Index : Offset) return String
      is
      begin
         --  An index of zero is used to represent an empty string, as the
         --  first word of the string table is specified to contain the length
         --  of the table rather than its contents.

         if Index = 0 then
            return "";

         else
            return Offset_To_String (Obj.Symstr_Stream, Index);
         end if;
      end String_Table;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : in out PECOFF_Object_File;
         Sym : Object_Symbol) return String_Ptr_Len
      is
         ST_Entry : Symtab_Entry;

      begin
         Seek (Obj.Symtab_Stream, Sym.Off);
         Read_Raw (Obj.Symtab_Stream, ST_Entry'Address, ST_Entry'Size / SSU);

         declare
            --  Symbol table entries are packed and Table_Entry.Name may not be
            --  sufficiently aligned to interpret as a 32 bit word, so it is
            --  copied to a temporary

            Aligned_Name : Name_Str := ST_Entry.Name;
            for Aligned_Name'Alignment use 4;

            First_Word : uint32;
            pragma Import (Ada, First_Word);
            --  Suppress initialization in Normalized_Scalars mode
            for First_Word'Address use Aligned_Name (1)'Address;

            Second_Word : uint32;
            pragma Import (Ada, Second_Word);
            --  Suppress initialization in Normalized_Scalars mode
            for Second_Word'Address use Aligned_Name (5)'Address;

         begin
            if First_Word = 0 then
               --  Second word is an offset in the symbol table
               if Second_Word = 0 then
                  return (null, 0);
               else
                  Seek (Obj.Symstr_Stream, int64 (Second_Word));
                  return Read (Obj.Symstr_Stream);
               end if;
            else
               --  Inlined symbol name
               Seek (Obj.Symtab_Stream, Sym.Off);
               return To_String_Ptr_Len (Read (Obj.Symtab_Stream), 8);
            end if;
         end;
      end Name;

   end PECOFF_Ops;

   -----------------
   -- XCOFF32_Ops --
   -----------------

   package body XCOFF32_Ops is

      function Read_Section_Header
        (Obj   : in out XCOFF32_Object_File;
         Index : uint32) return Section_Header;
      --  Read a header from section table

      -----------------
      -- Read_Symbol --
      -----------------

      function Read_Symbol
        (Obj : in out XCOFF32_Object_File;
         Off : Offset) return Object_Symbol
      is
         Sym     : Symbol_Entry;
         Sz      : constant Offset := Symbol_Entry'Size / SSU;
         Aux     : Aux_Entry;
         Result  : Object_Symbol;
         Noff    : Offset;
         Sym_Off : Offset;

         procedure Read_LD_Symbol;
         --  Read the next LD symbol

         --------------------
         -- Read_LD_Symbol --
         --------------------

         procedure Read_LD_Symbol is
         begin
            loop
               Sym_Off := Noff;

               Read_Raw (Obj.Symtab_Stream, Sym'Address, uint32 (Sz));

               Noff := Noff + Offset (1 + Sym.n_numaux) * Sz;

               for J in 1 .. Sym.n_numaux loop
                  Read_Raw (Obj.Symtab_Stream, Aux'Address, uint32 (Sz));
               end loop;

               exit when Noff >= Obj.Symtab_Last;

               exit when Sym.n_numaux = 1
                 and then Sym.n_scnum /= 0
                 and then (Sym.n_sclass = C_EXT
                           or else Sym.n_sclass = C_HIDEXT
                           or else Sym.n_sclass = C_WEAKEXT)
                 and then Aux.x_smtyp = XTY_LD;
            end loop;
         end Read_LD_Symbol;

      --  Start of processing for Read_Symbol

      begin
         Seek (Obj.Symtab_Stream, Off);
         Noff := Off;
         Read_LD_Symbol;

         if Noff >= Obj.Symtab_Last then
            return Null_Symbol;
         end if;

         --  Construct the symbol

         Result := (Off   => Sym_Off,
                    Next  => Noff,
                    Value => uint64 (Sym.n_value),
                    Size  => 0);

         --  Look for the next symbol to compute the size

         Read_LD_Symbol;

         if Noff >= Obj.Symtab_Last then
            return Null_Symbol;
         end if;

         Result.Size := uint64 (Sym.n_value) - Result.Value;
         Result.Next := Sym_Off;
         return Result;
      end Read_Symbol;

      ------------------
      -- First_Symbol --
      ------------------

      function First_Symbol
        (Obj : in out XCOFF32_Object_File) return Object_Symbol
      is
      begin
         --  Return Null_Symbol in the case that the symbol table is empty

         if Obj.Symtab_Last = 0 then
            return Null_Symbol;
         end if;

         return Read_Symbol (Obj, 0);
      end First_Symbol;

      ----------------
      -- Initialize --
      ----------------

      function Initialize
        (F            : Mapped_File;
         Hdr          : Header;
         In_Exception : Boolean) return XCOFF32_Object_File
      is
         Res : XCOFF32_Object_File (Format => XCOFF32);
         Strtab_Sz : uint32;

      begin
         Res.MF := F;
         Res.In_Exception := In_Exception;

         Res.Arch := PPC;

         --  Map sections table
         Res.Num_Sections := uint32 (Hdr.f_nscns);
         Res.Sectab_Stream := Create_Stream
           (F,
            File_Size (Header'Size / SSU) + File_Size (Hdr.f_opthdr),
            File_Size (Hdr.f_nscns) * (Section_Header'Size / SSU));

         --  Map symbols table
         Res.Symtab_Last := Offset (Hdr.f_nscns) * (Symbol_Entry'Size / SSU);
         Res.Symtab_Stream := Create_Stream
           (F,
            File_Size (Hdr.f_symptr),
            File_Size (Res.Symtab_Last) + 4);

         --  Map string table
         Seek (Res.Symtab_Stream, Res.Symtab_Last);
         Strtab_Sz := Read (Res.Symtab_Stream);
         Res.Symstr_Stream := Create_Stream
           (F,
            File_Size (Res.Symtab_Last) + 4,
            File_Size (Strtab_Sz) - 4);

         return Res;
      end Initialize;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj   : in out XCOFF32_Object_File;
         Index : uint32) return Object_Section
      is
         Sec : constant Section_Header := Read_Section_Header (Obj, Index);

      begin
         return (Index, Offset (Sec.s_scnptr),
                 uint64 (Sec.s_vaddr),
                 uint64 (Sec.s_size),
                 (Sec.s_flags and STYP_TEXT) /= 0);
      end Get_Section;

      -----------------
      -- Read_Header --
      -----------------

      function Read_Header (F : in out Mapped_Stream) return Header is
         Hdr : Header;

      begin
         Seek (F, 0);
         Read_Raw (F, Hdr'Address, uint32 (Hdr'Size / SSU));
         return Hdr;
      end Read_Header;

      -------------------------
      -- Read_Section_Header --
      -------------------------

      function Read_Section_Header
        (Obj   : in out XCOFF32_Object_File;
         Index : uint32) return Section_Header
      is
         Sec : Section_Header;

      begin
         --  Seek to the end of the object header

         Seek (Obj.Sectab_Stream, Offset (Index * Section_Header'Size / SSU));

         --  Read the section

         Read_Raw (Obj.Sectab_Stream, Sec'Address, Section_Header'Size / SSU);

         return Sec;
      end Read_Section_Header;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : in out XCOFF32_Object_File;
         Sec : Object_Section) return String
      is
         Hdr : Section_Header;

      begin
         Hdr := Read_Section_Header (Obj, Sec.Num);
         return Trim_Trailing_Nuls (Hdr.s_name);
      end Name;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : in out XCOFF32_Object_File;
         Sym : Object_Symbol) return String_Ptr_Len
      is
         Symbol : Symbol_Entry;

      begin
         Seek (Obj.Symtab_Stream, Sym.Off);
         Read_Raw (Obj.Symtab_Stream, Symbol'Address, Symbol'Size / SSU);

         declare
            First_Word : uint32;
            pragma Import (Ada, First_Word);
            --  Suppress initialization in Normalized_Scalars mode
            for First_Word'Address use Symbol.n_name (1)'Address;

            Second_Word : uint32;
            pragma Import (Ada, Second_Word);
            --  Suppress initialization in Normalized_Scalars mode
            for Second_Word'Address use Symbol.n_name (5)'Address;

         begin
            if First_Word = 0 then
               if Second_Word = 0 then
                  return (null, 0);
               else
                  Seek (Obj.Symstr_Stream, int64 (Second_Word));
                  return Read (Obj.Symstr_Stream);
               end if;
            else
               Seek (Obj.Symtab_Stream, Sym.Off);
               return To_String_Ptr_Len (Read (Obj.Symstr_Stream), 8);
            end if;
         end;
      end Name;
   end XCOFF32_Ops;

   ----------------
   -- MACH_O_Ops --
   ----------------

   package body MACH_O_Ops is

      pragma Warnings (Off, "is not referenced");

      --  At the beginning of every Mach-O file is a header structure
      --  that identifies the file as a Mach-O file. The header also
      --  contains other basic file type information, indicates the
      --  target architecture, and contains flags specifying options
      --  that affect the interpretation of the rest of the file.

      --  Directly following the header are a series of variable-size
      --  load commands that specify the layout and linkage
      --  characteristics of the file. Among other information, the
      --  load commands can specify:
      --  * The initial layout of the file in virtual memory
      --  * The location of the symbol table (used for dynamic linking)
      --  * The initial execution state of the main thread of the program
      --  * The names of shared libraries that contain definitions
      --    for the main executable’s imported symbols

      --  Following the load commands, all Mach-O files contain the
      --  data of one or more segments. Each segment contains zero or
      --  more sections. Each section of a segment contains code or
      --  data of some particular type. Each segment defines a region
      --  of virtual memory that the dynamic linker maps into the
      --  address space of the process. The exact number and layout of
      --  segments and sections is specified by the load commands and
      --  the file type.

      --  In user-level fully linked Mach-O files, the last segment is
      --  the link edit segment. This segment contains the tables of
      --  link edit information, such as the symbol table, string
      --  table, and so forth, used by the dynamic loader to link an
      --  executable file or Mach-O bundle to its dependent libraries.

      --  Various tables within a Mach-O file refer to sections by
      --  number. Section numbering begins at 1 (not 0) and continues
      --  across segment boundaries. Thus, the first segment in a file
      --  may contain sections 1 and 2 and the second segment may
      --  contain sections 3 and 4.

      --  When using the Stabs debugging format, the symbol table also
      --  holds debugging information. When using DWARF, debugging
      --  information is stored in the image’s corresponding dSYM
      --  file, specified by the uuid_command structure.

      --  MH_MAGIC_64 : constant := 16#feed_facf#;

      --  type mach_header_64 is record
      --     magic      : aliased uint32;
      --     cputype    : aliased int32;
      --     cpusubtype : aliased int32;
      --     filetype   : aliased uint32;
      --     ncmds      : aliased uint32;
      --     sizeofcmds : aliased uint32;
      --     flags      : aliased uint32;
      --     reserved   : aliased uint32;
      --  end record with
      --    Convention => C_Pass_By_Copy;

      type load_command_kind is
        (LC_SEGMENT,
         LC_SYMTAB,
         LC_SYMSEG,
         LC_THREAD,
         LC_UNIXTHREAD,
         LC_LOADFVMLIB,
         LC_IDFVMLIB,
         LC_IDENT,
         LC_FVMFILE,
         LC_PREPAGE,
         LC_DYSYMTAB,
         LC_LOAD_DYLIB,
         LC_ID_DYLIB,
         LC_LOAD_DYLINKER,
         LC_ID_DYLINKER,
         LC_PREBOUND_DYLIB,
         LC_ROUTINES,
         LC_SUB_FRAMEWORK,
         LC_SUB_UMBRELLA,
         LC_SUB_CLIENT,
         LC_SUB_LIBRARY,
         LC_TWOLEVEL_HINTS,
         LC_PREBIND_CKSUM,
         LC_LOAD_WEAK_DYLIB_x,
         LC_SEGMENT_64,
         LC_ROUTINES_64,
         LC_UUID,
         LC_RPATH_x,
         LC_CODE_SIGNATURE,
         LC_SEGMENT_SPLIT_INFO,
         LC_REEXPORT_DYLIB_x,
         LC_LAZY_LOAD_DYLIB,
         LC_ENCRYPTION_INFO,
         LC_DYLD_INFO,
         --  LC_DYLD_INFO_ONLY_x,
         LC_LOAD_UPWARD_DYLIB_x,
         LC_VERSION_MIN_MACOSX,
         LC_VERSION_MIN_IPHONEOS,
         LC_FUNCTION_STARTS,
         LC_DYLD_ENVIRONMENT,
         LC_MAIN_x,
         LC_DATA_IN_CODE,
         LC_SOURCE_VERSION,
         LC_DYLIB_CODE_SIGN_DRS,
         LC_ENCRYPTION_INFO_64,
         LC_LINKER_OPTION,
         LC_LINKER_OPTIMIZATION_HINT,
         LC_VERSION_MIN_TVOS,
         LC_VERSION_MIN_WATCHOS,
         LC_NOTE,
         LC_BUILD_VERSION,
         LC_DYLD_EXPORTS_TRIE_x,
         LC_DYLD_CHAINED_FIXUPS_x,
         LC_FILESET_ENTRY_x,
         LC_ATOM_INFO
         );

      for load_command_kind use
        (LC_SEGMENT                  => 16#1#,
         LC_SYMTAB                   => 16#2#,
         LC_SYMSEG                   => 16#3#,
         LC_THREAD                   => 16#4#,
         LC_UNIXTHREAD               => 16#5#,
         LC_LOADFVMLIB               => 16#6#,
         LC_IDFVMLIB                 => 16#7#,
         LC_IDENT                    => 16#8#,
         LC_FVMFILE                  => 16#9#,
         LC_PREPAGE                  => 16#a#,
         LC_DYSYMTAB                 => 16#b#,
         LC_LOAD_DYLIB               => 16#c#,
         LC_ID_DYLIB                 => 16#d#,
         LC_LOAD_DYLINKER            => 16#e#,
         LC_ID_DYLINKER              => 16#f#,
         LC_PREBOUND_DYLIB           => 16#10#,
         LC_ROUTINES                 => 16#11#,
         LC_SUB_FRAMEWORK            => 16#12#,
         LC_SUB_UMBRELLA             => 16#13#,
         LC_SUB_CLIENT               => 16#14#,
         LC_SUB_LIBRARY              => 16#15#,
         LC_TWOLEVEL_HINTS           => 16#16#,
         LC_PREBIND_CKSUM            => 16#17#,
         LC_LOAD_WEAK_DYLIB_x        => 16#18#,
         LC_SEGMENT_64               => 16#19#,
         LC_ROUTINES_64              => 16#1a#,
         LC_UUID                     => 16#1b#,
         LC_RPATH_x                  => 16#1c#,
         LC_CODE_SIGNATURE           => 16#1d#,
         LC_SEGMENT_SPLIT_INFO       => 16#1e#,
         LC_REEXPORT_DYLIB_x         => 16#1f#,
         LC_LAZY_LOAD_DYLIB          => 16#20#,
         LC_ENCRYPTION_INFO          => 16#21#,
         LC_DYLD_INFO                => 16#22#,
         LC_LOAD_UPWARD_DYLIB_x      => 16#23#,
         LC_VERSION_MIN_MACOSX       => 16#24#,
         LC_VERSION_MIN_IPHONEOS     => 16#25#,
         LC_FUNCTION_STARTS          => 16#26#,
         LC_DYLD_ENVIRONMENT         => 16#27#,
         LC_MAIN_x                   => 16#28#,
         LC_DATA_IN_CODE             => 16#29#,
         LC_SOURCE_VERSION           => 16#2A#,
         LC_DYLIB_CODE_SIGN_DRS      => 16#2B#,
         LC_ENCRYPTION_INFO_64       => 16#2C#,
         LC_LINKER_OPTION            => 16#2D#,
         LC_LINKER_OPTIMIZATION_HINT => 16#2E#,
         LC_VERSION_MIN_TVOS         => 16#2F#,
         LC_VERSION_MIN_WATCHOS      => 16#30#,
         LC_NOTE                     => 16#31#,
         LC_BUILD_VERSION            => 16#32#,
         LC_DYLD_EXPORTS_TRIE_x      => 16#33#,
         LC_DYLD_CHAINED_FIXUPS_x    => 16#34#,
         LC_FILESET_ENTRY_x          => 16#35#,
         LC_ATOM_INFO                => 16#36#
         );

      type load_command is record
         dyld_must_recognise : Boolean;
         cmd                 : load_command_kind;
         cmdsize             : aliased uint32;
      end record with
        Convention => C_Pass_By_Copy;
      for load_command use record
         dyld_must_recognise at 0 range 31 .. 31;
         cmd                 at 0 range  0 .. 30;
         cmdsize             at 4 range  0 .. 31;
      end record;

      type lc_str (discr : unsigned := 0) is record
         case discr is
            when others =>
               offset : aliased uint32;
         end case;
      end record with
        Convention => C_Pass_By_Copy, Unchecked_Union => True;

      --  More load commands below.

      subtype anon_array1770 is Interfaces.C.char_array (0 .. 15);

      type segment_command_64 is record
         cmd      : aliased uint32;
         cmdsize  : aliased uint32;
         segname  : aliased anon_array1770;
         vmaddr   : aliased uint64;
         vmsize   : aliased uint64;
         fileoff  : aliased uint64;
         filesize : aliased uint64;
         maxprot  : aliased int32;
         initprot : aliased int32;
         nsects   : aliased uint32;
         flags    : aliased uint32;
      end record with
        Convention => C_Pass_By_Copy;

      --  These follow after a segment_64_command
      type section_64 is record
         sectname  : aliased anon_array1770;
         segname   : aliased anon_array1770;
         addr      : aliased uint64;
         size      : aliased uint64;
         offset    : aliased uint32;
         align     : aliased uint32;
         reloff    : aliased uint32;
         nreloc    : aliased uint32;
         flags     : aliased uint32;
         reserved1 : aliased uint32;
         reserved2 : aliased uint32;
         reserved3 : aliased uint32;
      end record with
        Convention => C_Pass_By_Copy;

      type symtab_command is record
         cmd     : aliased uint32;
         cmdsize : aliased uint32;
         symoff  : aliased uint32;
         nsyms   : aliased uint32;
         stroff  : aliased uint32;
         strsize : aliased uint32;
      end record
      with Convention => C_Pass_By_Copy;

      --  nlist_64 items: referenced in symtab_command (but separate,
      --  unlike section_64 items)

      type anon_anon_2 (discr : unsigned := 0) is record
         case discr is
            when others =>
               n_strx : aliased uint32;
         end case;
      end record
      with Convention   => C_Pass_By_Copy,
        Unchecked_Union => True;

      type nlist_64 is record
         n_un    : aliased anon_anon_2;
         n_type  : aliased uint8;
         n_sect  : aliased uint8;
         n_desc  : aliased uint16;
         n_value : aliased uint64;
      end record
      with Convention => C_Pass_By_Copy;

      Unimplemented : exception;

      function Read_Header (F : in out Mapped_Stream) return Header is
         Hdr : Header;
      begin
         Seek (F, 0);
         Read_Raw (F, Hdr'Address, uint32 (Hdr'Size / SSU));
         GNAT.IO.Put_Line (Hdr'Image);
         GNAT.IO.New_Line;
         return Hdr;
      end Read_Header;

      function First_Symbol
        (Obj : in out MACH_O_Object_File) return Object_Symbol is
        (raise Unimplemented);

      function Read_Symbol
        (Obj : in out MACH_O_Object_File; Off : Offset)
         return Object_Symbol is
        (raise Unimplemented);

      function Name
        (Obj : in out MACH_O_Object_File; Sym : Object_Symbol)
         return String_Ptr_Len is
        (raise Unimplemented);

      function Name
        (Obj : in out MACH_O_Object_File; Sec : Object_Section)
         return String is
        (raise Unimplemented);

      function Initialize
        (F : Mapped_File; Hdr : Header; In_Exception : Boolean)
         return MACH_O_Object_File
      is
         Command_Stream : Mapped_Stream :=
           Create_Stream
             (MF          => F,
              File_Offset => 0,
              File_Length => Length (F));
         unused_hdr     : Header;
         Location       : int64;
         Cmd            : load_command;
      begin
         --  Read the header again, so that Command_Stream started at
         --  the beginning of the executable file, but is positioned
         --  after the header.
         unused_hdr := Read_Header (Command_Stream);
         Location   := Tell (Command_Stream);
         for c in 1 .. Hdr.ncmds loop
            Seek (Command_Stream, Location);
            --  Read the common header
            Read_Raw
              (Command_Stream, Cmd'Address, uint32 (Cmd'Size / SSU));
            GNAT.IO.Put_Line ("lc " & Cmd.cmd'Image);
            --  Go back to the start of the command
            Seek (Command_Stream, Location);
            case Cmd.cmd is
               when LC_SEGMENT_64 =>
                  declare
                     cmd : segment_command_64;
                  begin
                     Read_Raw
                       (Command_Stream, cmd'Address, uint32 (cmd'Size / SSU));
                     GNAT.IO.Put_Line (cmd'Image);
                     GNAT.IO.New_Line;
                     for j in 1 .. cmd.nsects loop
                        declare
                           sec : section_64;
                        begin
                           Read_Raw
                             (Command_Stream,
                              sec'Address,
                              uint32 (sec'Size / SSU));
                           GNAT.IO.Put_Line ("section" & j'Image);
                           GNAT.IO.Put_Line (sec'Image);
                           GNAT.IO.New_Line;
                        end;
                     end loop;
                  end;
               when LC_SYMTAB =>
                  declare
                     cmd : symtab_command;
                  begin
                     Read_Raw
                       (Command_Stream, cmd'Address, uint32 (cmd'Size / SSU));
                     GNAT.IO.Put_Line (cmd'Image);
                     GNAT.IO.New_Line;
                     Seek (Command_Stream, Offset (cmd.symoff));
                     for j in 1 .. Integer'Min (5, Integer (cmd.nsyms)) loop
                        declare
                           sym          : nlist_64;
                           str_offset   : uint32;
                           continuation : Offset;
                        begin
                           Read_Raw
                             (Command_Stream,
                              sym'Address,
                              uint32 (sym'Size / SSU));
                           GNAT.IO.Put
                             ("sym" &
                              j'Image);
                           str_offset := sym.n_un.n_strx;
                           GNAT.IO.Put_Line (", idx" & str_offset'Image);
                           GNAT.IO.Put_Line (sym'Image);
                           --  Remember where we'd got to, so we can
                           --  read the next symbol.
                           continuation := Tell (Command_Stream);

                           GNAT.IO.Put_Line
                             (Offset_To_String
                                (Command_Stream,
                                  Offset (cmd.stroff + str_offset)));
                           GNAT.IO.New_Line;

                           --  Ready for the next symbol.
                           Seek (Command_Stream, continuation);
                        end;
                     end loop;
                  end;
               when others =>
                  null;
            end case;
            Location := Location + int64 (Cmd.cmdsize);
         end loop;
         return raise Unimplemented;
      end Initialize;

      function Get_Section
        (Obj : in out MACH_O_Object_File; Index : uint32)
         return Object_Section is
        (raise Unimplemented);

      pragma Warnings (On, "is not referenced");

   end MACH_O_Ops;

   ----------
   -- Arch --
   ----------

   function Arch (Obj : Object_File) return Object_Arch is
   begin
      return Obj.Arch;
   end Arch;

   function Create_Stream
     (MF : Mapped_File;
      File_Offset : File_Size;
      File_Length : File_Size)
     return Mapped_Stream
   is
      Region : Mapped_Region;
   begin
      Read (MF, Region, File_Offset, File_Length, False);
      return (Region, 0, Offset (File_Length));
   end Create_Stream;

   function Create_Stream
     (Obj : Object_File;
      Sec : Object_Section) return Mapped_Stream
   is
   begin
      return Create_Stream (Obj.MF, File_Size (Sec.Off), File_Size (Sec.Size));
   end Create_Stream;

   procedure Tell (Obj : in out Mapped_Stream; Off : out Offset) is
   begin
      Off := Obj.Off;
   end Tell;

   function Tell (Obj : Mapped_Stream) return Offset is
   begin
      return Obj.Off;
   end Tell;

   function Length (Obj : Mapped_Stream) return Offset is
   begin
      return Obj.Len;
   end Length;

   -----------
   -- Close --
   -----------

   procedure Close (S : in out Mapped_Stream) is
   begin
      Free (S.Region);
   end Close;

   procedure Close (Obj : in out Object_File) is
   begin
      Close (Obj.Symtab_Stream);
      Close (Obj.Symstr_Stream);
      Close (Obj.Sectab_Stream);

      case Obj.Format is
         when ELF | MACH_O =>
            Close (Obj.Secstr_Stream);
         when Any_PECOFF =>
            null;
         when XCOFF32 =>
            null;
      end case;

      Close (Obj.MF);
   end Close;

   ------------------------
   -- Strip_Leading_Char --
   ------------------------

   function Strip_Leading_Char
     (Obj : in out Object_File;
      Sym : String_Ptr_Len) return Positive
   is
   begin
      if (Obj.Format = PECOFF  and then Sym.Ptr (1) = '_')
        or else
        (Obj.Format = XCOFF32 and then Sym.Ptr (1) = '.')
      then
         return 2;
      else
         return 1;
      end if;
   end Strip_Leading_Char;

   ----------------------
   -- Decoded_Ada_Name --
   ----------------------

   function Decoded_Ada_Name
     (Obj : in out Object_File;
      Sym : String_Ptr_Len) return String
   is
      procedure gnat_decode
        (Coded_Name_Addr : Address;
         Ada_Name_Addr   : Address;
         Verbose         : int);
      pragma Import (C, gnat_decode, "__gnat_decode");

      subtype size_t is Interfaces.C.size_t;

      Sym_Name : constant String :=
        String (Sym.Ptr (1 .. Sym.Len)) & ASCII.NUL;
      Decoded : char_array (0 .. size_t (Sym.Len) * 2 + 60);
      Off     : Natural;

   begin
      --  In the PECOFF case most but not all symbol table entries have an
      --  extra leading underscore. In this case we trim it.

      Off := Strip_Leading_Char (Obj, Sym);

      gnat_decode (Sym_Name (Off)'Address, Decoded'Address, 0);

      return To_Ada (Decoded);
   end Decoded_Ada_Name;

   ------------------
   -- First_Symbol --
   ------------------

   function First_Symbol (Obj : in out Object_File) return Object_Symbol is
   begin
      case Obj.Format is
         when ELF32      => return ELF32_Ops.First_Symbol   (Obj);
         when ELF64      => return ELF64_Ops.First_Symbol   (Obj);
         when Any_PECOFF => return PECOFF_Ops.First_Symbol  (Obj);
         when XCOFF32    => return XCOFF32_Ops.First_Symbol (Obj);
         when MACH_O     => return MACH_O_Ops.First_Symbol  (Obj);
      end case;
   end First_Symbol;

   ------------
   -- Format --
   ------------

   function Format (Obj : Object_File) return Object_Format is
   begin
      return Obj.Format;
   end Format;

   ----------------------
   -- Get_Load_Address --
   ----------------------

   function Get_Load_Address (Obj : Object_File) return uint64 is
   begin
      case Obj.Format is
         when ELF        => return 0;
         when Any_PECOFF => return Obj.ImageBase;
         when XCOFF32    => raise Format_Error;
         when MACH_O     => return Obj.ImageBase;
      end case;
   end Get_Load_Address;

   -----------------
   -- Get_Section --
   -----------------

   function Get_Section
     (Obj   : in out Object_File;
      Shnum : uint32) return Object_Section
   is
   begin
      case Obj.Format is
         when ELF32      => return ELF32_Ops.Get_Section   (Obj, Shnum);
         when ELF64      => return ELF64_Ops.Get_Section   (Obj, Shnum);
         when Any_PECOFF => return PECOFF_Ops.Get_Section  (Obj, Shnum);
         when XCOFF32    => return XCOFF32_Ops.Get_Section (Obj, Shnum);
         when MACH_O     => return MACH_O_Ops.Get_Section  (Obj, Shnum);
      end case;
   end Get_Section;

   function Get_Section
     (Obj      : in out Object_File;
      Sec_Name : String) return Object_Section
   is
      Sec : Object_Section;

   begin
      for J in 0 .. Obj.Num_Sections - 1 loop
         Sec := Get_Section (Obj, J);

         if Name (Obj, Sec) = Sec_Name then
            return Sec;
         end if;
      end loop;

      if Obj.In_Exception then
         return Null_Section;
      else
         raise Format_Error with "could not find section in object file";
      end if;
   end Get_Section;

   ----------------------
   -- Get_Xcode_Bounds --
   ----------------------

   procedure Get_Xcode_Bounds
     (Obj       : in out Object_File;
      Low, High : out uint64)
   is
      Sec : Object_Section;

   begin
      --  First set as an empty range
      Low := uint64'Last;
      High := uint64'First;

      --  Now find the lowest and highest offsets
      --  attached to executable code sections
      for Idx in 1 .. Num_Sections (Obj) loop
         Sec := Get_Section (Obj, Idx - 1);
         if Sec.Flag_Xcode then
            if Sec.Addr < Low then
               Low := Sec.Addr;
            end if;
            if Sec.Addr + Sec.Size > High then
               High := Sec.Addr + Sec.Size;
            end if;
         end if;
      end loop;
   end Get_Xcode_Bounds;

   ----------
   -- Name --
   ----------

   function Name
     (Obj : in out Object_File;
      Sec : Object_Section) return String
   is
   begin
      case Obj.Format is
         when ELF32      => return ELF32_Ops.Name   (Obj, Sec);
         when ELF64      => return ELF64_Ops.Name   (Obj, Sec);
         when Any_PECOFF => return PECOFF_Ops.Name  (Obj, Sec);
         when XCOFF32    => return XCOFF32_Ops.Name (Obj, Sec);
         when MACH_O     => return MACH_O_Ops.Name  (Obj, Sec);
      end case;
   end Name;

   function Name
     (Obj : in out Object_File;
      Sym : Object_Symbol) return String_Ptr_Len
   is
   begin
      case Obj.Format is
         when ELF32      => return ELF32_Ops.Name   (Obj, Sym);
         when ELF64      => return ELF64_Ops.Name   (Obj, Sym);
         when Any_PECOFF => return PECOFF_Ops.Name  (Obj, Sym);
         when XCOFF32    => return XCOFF32_Ops.Name (Obj, Sym);
         when MACH_O     => return MACH_O_Ops.Name  (Obj, Sym);
      end case;
   end Name;

   -----------------
   -- Next_Symbol --
   -----------------

   function Next_Symbol
     (Obj  : in out Object_File;
      Prev : Object_Symbol) return Object_Symbol
   is
   begin
      --  Test whether we've reached the end of the symbol table

      if Prev.Next >= Obj.Symtab_Last then
         return Null_Symbol;
      end if;

      return Read_Symbol (Obj, Prev.Next);
   end Next_Symbol;

   ---------
   -- Num --
   ---------

   function Num (Sec : Object_Section) return uint32 is
   begin
      return Sec.Num;
   end Num;

   ------------------
   -- Num_Sections --
   ------------------

   function Num_Sections (Obj : Object_File) return uint32 is
   begin
      return Obj.Num_Sections;
   end Num_Sections;

   ---------
   -- Off --
   ---------

   function Off (Sec : Object_Section) return Offset is
   begin
      return Sec.Off;
   end Off;

   function Off (Sym : Object_Symbol) return Offset is
   begin
      return Sym.Off;
   end Off;

   ----------------------
   -- Offset_To_String --
   ----------------------

   function Offset_To_String
     (S : in out Mapped_Stream;
      Off : Offset) return String
   is
      Buf : Buffer;

   begin
      Seek (S, Off);
      Read_C_String (S, Buf);
      return To_String (Buf);
   end Offset_To_String;

   ----------
   -- Open --
   ----------

   function Open
     (File_Name    : String;
      In_Exception : Boolean := False) return Object_File_Access
   is
      F          : Mapped_File;
      Hdr_Stream : Mapped_Stream;

   begin
      --  Open the file

      F := Open_Read_No_Exception (File_Name);

      if F = Invalid_Mapped_File then
         if In_Exception then
            return null;
         else
            raise IO_Error with "could not open object file";
         end if;
      end if;

      Hdr_Stream := Create_Stream (F, 0, 4096);

      declare
         Hdr : constant ELF32_Ops.Header := ELF32_Ops.Read_Header (Hdr_Stream);

      begin
         --  Look for the magic numbers for the ELF case

         if Hdr.E_Ident (0) = 16#7F#              and then
            Hdr.E_Ident (1) = Character'Pos ('E') and then
            Hdr.E_Ident (2) = Character'Pos ('L') and then
            Hdr.E_Ident (3) = Character'Pos ('F') and then
            Hdr.E_Ident (4) = ELF32_Ops.ELFCLASS32
         then
            Close (Hdr_Stream);
            return new Object_File'
                  (ELF32_Ops.Initialize (F, Hdr, In_Exception));
         end if;
      end;

      declare
         Hdr : constant ELF64_Ops.Header :=
           ELF64_Ops.Read_Header (Hdr_Stream);

      begin
         --  Look for the magic numbers for the ELF case

         if Hdr.E_Ident (0) = 16#7F#              and then
            Hdr.E_Ident (1) = Character'Pos ('E') and then
            Hdr.E_Ident (2) = Character'Pos ('L') and then
            Hdr.E_Ident (3) = Character'Pos ('F') and then
            Hdr.E_Ident (4) = ELF32_Ops.ELFCLASS64
         then
            Close (Hdr_Stream);
            return new Object_File'
                         (ELF64_Ops.Initialize (F, Hdr, In_Exception));
         end if;
      end;

      declare
         Hdr : constant PECOFF_Ops.Header :=
           PECOFF_Ops.Read_Header (Hdr_Stream);

      begin
         --  Test the magic numbers

         if Hdr.Magics (0) = Character'Pos ('P') and then
            Hdr.Magics (1) = Character'Pos ('E') and then
            Hdr.Magics (2) = 0                   and then
            Hdr.Magics (3) = 0
         then
            Close (Hdr_Stream);
            return new Object_File'
                         (PECOFF_Ops.Initialize (F, Hdr, In_Exception));
         end if;

      exception
         --  If this is not a PECOFF file then we've done a seek and read to a
         --  random address, possibly raising IO_Error

         when IO_Error =>
            null;
      end;

      declare
         Hdr : constant XCOFF32_Ops.Header :=
           XCOFF32_Ops.Read_Header (Hdr_Stream);

      begin
         --  Test the magic numbers

         if Hdr.f_magic = 8#0737# then
            Close (Hdr_Stream);
            return new Object_File'
                         (XCOFF32_Ops.Initialize (F, Hdr, In_Exception));
         end if;
      end;

      declare
         Hdr : constant MACH_O_Ops.Header :=
           MACH_O_Ops.Read_Header (Hdr_Stream);

      begin
         --  Test the magic numbers

         if Hdr.magic = MACH_O_Ops.MH_MAGIC_64 then
            Close (Hdr_Stream);
            return new Object_File'
                         (MACH_O_Ops.Initialize (F, Hdr, In_Exception));
         end if;
      end;

      Close (Hdr_Stream);

      if In_Exception then
         return null;
      else
         raise Format_Error with "unrecognized object format";
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   function Read (S : in out Mapped_Stream) return Mmap.Str_Access is
      function To_Str_Access is
         new Ada.Unchecked_Conversion (Address, Str_Access);

   begin
      return To_Str_Access (Data (S.Region) (Natural (S.Off + 1))'Address);
   end Read;

   function Read (S : in out Mapped_Stream) return String_Ptr_Len is
   begin
      return To_String_Ptr_Len (Read (S));
   end Read;

   procedure Check_Read_Offset (S : Mapped_Stream; Size : uint32) is
   begin
      if S.Off + Offset (Size) > Offset (Last (S.Region)) then
         raise IO_Error with "could not read from object file";
      end if;
   end Check_Read_Offset;

   procedure Read_Raw
     (S    : in out Mapped_Stream;
      Addr : Address;
      Size : uint32)
   is
      function To_Str_Access is
         new Ada.Unchecked_Conversion (Address, Str_Access);
      Sz : constant Offset := Offset (Size);

   begin
      --  Check size

      pragma Debug (Check_Read_Offset (S, Size));

      --  Copy data

      To_Str_Access (Addr) (1 .. Positive (Sz)) :=
        Data (S.Region) (Positive (S.Off + 1) .. Positive (S.Off + Sz));

      --  Update offset

      S.Off := S.Off + Sz;
   end Read_Raw;

   function Read (S : in out Mapped_Stream) return uint8 is
      Data : uint8;
   begin
      Read_Raw (S, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (S : in out Mapped_Stream) return uint16 is
      Data : uint16;
   begin
      Read_Raw (S, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (S : in out Mapped_Stream) return uint32 is
      Data : uint32;
   begin
      Read_Raw (S, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (S : in out Mapped_Stream) return uint64 is
      Data : uint64;
   begin
      Read_Raw (S, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (S : in out Mapped_Stream) return int8 is
      Data : int8;
   begin
      Read_Raw (S, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (S : in out Mapped_Stream) return int16 is
      Data : int16;
   begin
      Read_Raw (S, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (S : in out Mapped_Stream) return int32 is
      Data : int32;
   begin
      Read_Raw (S, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (S : in out Mapped_Stream) return int64 is
      Data : int64;
   begin
      Read_Raw (S, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   ------------------
   -- Read_Address --
   ------------------

   function Read_Address
     (Obj : Object_File; S : in out Mapped_Stream) return uint64
   is
      Address_32 : uint32;
      Address_64 : uint64;

   begin
      case Obj.Arch is
         when i386
            | MIPS
            | PPC
            | SPARC
            | ARM
         =>
            Address_32 := Read (S);
            return uint64 (Address_32);

         when AARCH64
            | IA64
            | PPC64
            | SPARC64
            | x86_64
         =>
            Address_64 := Read (S);
            return Address_64;

         when Unknown =>
            raise Format_Error with "unrecognized machine architecture";
      end case;
   end Read_Address;

   -------------------
   -- Read_C_String --
   -------------------

   procedure Read_C_String (S : in out Mapped_Stream; B : out Buffer) is
      J : Integer := 0;

   begin
      loop
         --  Handle overflow case

         if J = B'Last then
            B (J) := 0;
            exit;
         end if;

         B (J) := Read (S);
         exit when B (J) = 0;
         J := J + 1;
      end loop;
   end Read_C_String;

   -------------------
   -- Read_C_String --
   -------------------

   function Read_C_String (S : in out Mapped_Stream) return Str_Access is
      Res : constant Str_Access := Read (S);

   begin
      for J in Res'Range loop
         if S.Off + Offset (J - 1) > Offset (Last (S.Region)) then
            raise IO_Error with "could not read from object file";
         end if;

         if Res (J) = ASCII.NUL then
            S.Off := S.Off + Offset (J);
            return Res;
         end if;
      end loop;

      --  Overflow case
      raise Constraint_Error;
   end Read_C_String;

   -----------------
   -- Read_LEB128 --
   -----------------

   function Read_LEB128 (S : in out Mapped_Stream) return uint32 is
      B     : uint8;
      Shift : Integer := 0;
      Res   : uint32 := 0;

   begin
      loop
         B := Read (S);
         Res := Res or Shift_Left (uint32 (B and 16#7f#), Shift);
         exit when (B and 16#80#) = 0;
         Shift := Shift + 7;
      end loop;

      return Res;
   end Read_LEB128;

   function Read_LEB128 (S : in out Mapped_Stream) return int32 is
      B     : uint8;
      Shift : Integer := 0;
      Res   : uint32 := 0;

   begin
      loop
         B := Read (S);
         Res := Res or Shift_Left (uint32 (B and 16#7f#), Shift);
         Shift := Shift + 7;
         exit when (B and 16#80#) = 0;
      end loop;

      if Shift < 32 and then (Res and Shift_Left (1, Shift - 1)) /= 0 then
         Res := Res or Shift_Left (-1, Shift);
      end if;

      return To_int32 (Res);
   end Read_LEB128;

   -----------------
   -- Read_Symbol --
   -----------------

   function Read_Symbol
     (Obj : in out Object_File;
      Off : Offset) return Object_Symbol
   is
   begin
      case Obj.Format is
         when ELF32      => return ELF32_Ops.Read_Symbol   (Obj, Off);
         when ELF64      => return ELF64_Ops.Read_Symbol   (Obj, Off);
         when Any_PECOFF => return PECOFF_Ops.Read_Symbol  (Obj, Off);
         when XCOFF32    => return XCOFF32_Ops.Read_Symbol (Obj, Off);
         when MACH_O     => return MACH_O_Ops.Read_Symbol  (Obj, Off);
      end case;
   end Read_Symbol;

   ----------
   -- Seek --
   ----------

   procedure Seek (S : in out Mapped_Stream; Off : Offset) is
   begin
      if Off < 0 or else Off > Offset (Last (S.Region)) then
         raise IO_Error with "could not seek to offset in object file";
      end if;

      S.Off := Off;
   end Seek;

   ----------
   -- Size --
   ----------

   function Size (Sec : Object_Section) return uint64 is
   begin
      return Sec.Size;
   end Size;

   function Size (Sym : Object_Symbol) return uint64 is
   begin
      return Sym.Size;
   end Size;

   ------------
   -- Strlen --
   ------------

   function Strlen (Buf : Buffer) return int32 is
   begin
      return int32 (CRTL.strlen (Buf'Address));
   end Strlen;

   -----------
   -- Spans --
   -----------

   function Spans (Sym : Object_Symbol; Addr : uint64) return Boolean is
   begin
      return Addr >= Sym.Value and then Addr < Sym.Value + Sym.Size;
   end Spans;

   ---------------
   -- To_String --
   ---------------

   function To_String (Buf : Buffer) return String is
      Result : String (1 .. Integer (CRTL.strlen (Buf'Address)));
      for Result'Address use Buf'Address;
      pragma Import (Ada, Result);

   begin
      return Result;
   end To_String;

   -----------------------
   -- To_String_Ptr_Len --
   -----------------------

   function To_String_Ptr_Len
     (Ptr : Mmap.Str_Access;
      Max_Len : Natural := Natural'Last) return String_Ptr_Len
   is
   begin
      for I in 1 .. Max_Len loop
         if Ptr (I) = ASCII.NUL then
            return (Ptr, I - 1);
         end if;
      end loop;
      return (Ptr, Max_Len);
   end To_String_Ptr_Len;

   ------------------------
   -- Trim_Trailing_Nuls --
   ------------------------

   function Trim_Trailing_Nuls (Str : String) return String is
   begin
      for J in Str'Range loop
         if Str (J) = ASCII.NUL then
            return Str (Str'First .. J - 1);
         end if;
      end loop;

      return Str;
   end Trim_Trailing_Nuls;

   -----------
   -- Value --
   -----------

   function Value (Sym : Object_Symbol) return uint64 is
   begin
      return Sym.Value;
   end Value;

end System.Object_Reader;
