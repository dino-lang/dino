# Microsoft Developer Studio Project File - Name="dino" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=dino - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "dino.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "dino.mak" CFG="dino - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "dino - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "dino - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "dino - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\bin"
# PROP Intermediate_Dir "..\obj\dino"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "NO_INTERFACE" /YX /FD /c
# ADD BASE RSC /l 0x1009 /d "NDEBUG"
# ADD RSC /l 0x1009 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "dino - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "dino___Win32_Debug"
# PROP BASE Intermediate_Dir "dino___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\bin"
# PROP Intermediate_Dir "..\obj\dino"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "NO_INTERFACE" /YX /FD /GZ /c
# ADD BASE RSC /l 0x1009 /d "_DEBUG"
# ADD RSC /l 0x1009 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "dino - Win32 Release"
# Name "dino - Win32 Debug"
# Begin Source File

SOURCE=.\d_blocktab.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_context.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# SUBTRACT CPP /u
# End Source File
# Begin Source File

SOURCE=.\d_conv.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_dino.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_errors.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_eval.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_extern.sprut

!IF  "$(CFG)" == "dino - Win32 Release"

# PROP Intermediate_Dir "..\dino\obj"
# Begin Custom Build - Making Interface SPI
InputPath=.\d_extern.sprut

"d_extern.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\bin\sprut -only-macro -pER_ -access -set d_extern.sprut

# End Custom Build

!ELSEIF  "$(CFG)" == "dino - Win32 Debug"

# PROP Intermediate_Dir "..\dino\obj"
# Begin Custom Build - Making Interface SPI
InputPath=.\d_extern.sprut

"d_extern.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\bin\sprut -only-macro -pER_ -access -set d_extern.sprut

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\d_func.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_ir.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_ir.sprut

!IF  "$(CFG)" == "dino - Win32 Release"

# PROP Intermediate_Dir "..\obj\dino"
# Begin Custom Build - Making Dino IR
InputPath=.\d_ir.sprut

BuildCmds= \
	..\bin\sprut -only-macro -access -set d_ir.sprut

"d_ir.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"d_ir.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "dino - Win32 Debug"

# PROP Intermediate_Dir "..\obj\dino"
# Begin Custom Build - Making Dino IR
InputPath=.\d_ir.sprut

BuildCmds= \
	..\bin\sprut -only-macro -access -set d_ir.sprut

"d_ir.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"d_ir.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\d_kw.shilka

!IF  "$(CFG)" == "dino - Win32 Release"

# PROP Intermediate_Dir "..\obj\dino"
# Begin Custom Build - Making Kw recognizer
InputPath=.\d_kw.shilka

"d_kw.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\bin\shilka -no-definitions -strip d_kw.shilka

# End Custom Build

!ELSEIF  "$(CFG)" == "dino - Win32 Debug"

# PROP Intermediate_Dir "..\obj\dino"
# Begin Custom Build - Making Kw recognizer
InputPath=.\d_kw.shilka

"d_kw.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\bin\shilka -no-definitions -strip d_kw.shilka

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\d_main.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_run.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_run.sprut

!IF  "$(CFG)" == "dino - Win32 Release"

# PROP Intermediate_Dir "..\obj\dino"
# Begin Custom Build - Making DINO Run-time IR
InputPath=.\d_run.sprut

"d_run.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\bin\sprut -only-macro -pER_ -access -set d_run.sprut

# End Custom Build

!ELSEIF  "$(CFG)" == "dino - Win32 Debug"

# PROP Intermediate_Dir "..\obj\dino"
# Begin Custom Build - Making DINO Run-time IR
InputPath=.\d_run.sprut

"d_run.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\bin\sprut -only-macro -pER_ -access -set d_run.sprut

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\d_yacc.c
# ADD CPP /I "..\ammunition" /I "..\regex" /D "NO_DINO_SHLIB"
# End Source File
# Begin Source File

SOURCE=.\d_yacc.y

!IF  "$(CFG)" == "dino - Win32 Release"

# PROP Intermediate_Dir "..\obj\dino"
# Begin Custom Build - Making Dino Parser
InputPath=.\d_yacc.y

"d_yacc.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\bin\msta -no-regular-optimization d_yacc.y

# End Custom Build

!ELSEIF  "$(CFG)" == "dino - Win32 Debug"

# PROP Intermediate_Dir "..\obj\dino"
# Begin Custom Build - Making Dino Parser
InputPath=.\d_yacc.y

"d_yacc.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	..\bin\msta -no-regular-optimization d_yacc.y

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Regex\regex.c
# ADD CPP /D "_POSIX_SOURCE" /D "STDC_HEADERS" /D "HAVE_STRING_H" /D "REGEX_MALLOC"
# End Source File
# End Target
# End Project
