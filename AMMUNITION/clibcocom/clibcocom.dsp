# Microsoft Developer Studio Project File - Name="clibcocom" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=clibcocom - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "clibcocom.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "clibcocom.mak" CFG="clibcocom - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "clibcocom - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "clibcocom - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "clibcocom - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "../../bin"
# PROP Intermediate_Dir "../../obj/clibcocom"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE RSC /l 0x1009
# ADD RSC /l 0x1009
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "clibcocom - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "../../bin"
# PROP Intermediate_Dir "../../obj/clibcocom"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE RSC /l 0x1009
# ADD RSC /l 0x1009
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "clibcocom - Win32 Release"
# Name "clibcocom - Win32 Debug"
# Begin Source File

SOURCE=..\allocate.c
# End Source File
# Begin Source File

SOURCE=..\arithm.c
# End Source File
# Begin Source File

SOURCE=..\bits.c
# End Source File
# Begin Source File

SOURCE=..\commline.c
# End Source File
# Begin Source File

SOURCE=..\earley.c
# End Source File
# Begin Source File

SOURCE=..\errors.c
# End Source File
# Begin Source File

SOURCE=..\hashtab.c
# End Source File
# Begin Source File

SOURCE=..\IEEE.c
# ADD CPP /D "IEEE_QUAD"
# End Source File
# Begin Source File

SOURCE=..\objstack.c
# End Source File
# Begin Source File

SOURCE=..\position.c
# End Source File
# Begin Source File

SOURCE=..\sgramm.y

!IF  "$(CFG)" == "clibcocom - Win32 Release"

# Begin Custom Build - YACC
InputPath=..\sgramm.y

"sgramm.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy/y ..\sgramm.y sgramm.y 
	..\..\yacc sgramm.y 
	copy/y ytab.c ..\sgramm.c 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "clibcocom - Win32 Debug"

# Begin Custom Build - YACC
InputPath=..\sgramm.y

"sgramm.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy/y ..\sgramm.y sgramm.y 
	..\..\yacc sgramm.y 
	copy/y ytab.c ..\sgramm.c 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\ticker.c
# End Source File
# Begin Source File

SOURCE=..\vlobject.c
# End Source File
# End Target
# End Project
