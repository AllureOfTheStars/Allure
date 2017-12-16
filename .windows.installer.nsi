!include "MUI2.nsh"

;--------------------------------
;General

SetCompressor /SOLID lzma
ShowInstDetails show
ShowUninstDetails show
SetDateSave on
CRCCheck on

!include "x64.nsh"
#Function .onInit
#StrCpy $instdir $programfiles32\Allure
#${If} ${RunningX64}
#  StrCpy $instdir $programfiles64\Allure
#${EndIf}
#FunctionEnd

Name "Allure of the Stars" # Name of the installer (usually the name of the application to install).

OutFile "Allure_dev_windows-installer.exe" # Name of the installer's file.

InstallDir "$LOCALAPPDATA\Allure" # Default installing folder ($PROGRAMFILES is Program Files folder, but requires admin rights, so I pick some local folder instead).

; Registry key to check for directory (so if you install again, it will
; overwrite the old one automatically)
  InstallDirRegKey HKLM "Software\Allure" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel user

;--------------------------------
;Variables

  Var StartMenuFolder

;--------------------------------
;Interface Settings

!define MUI_ICON favicon.ico
!define MUI_UNICON favicon.ico
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_UNFINISHPAGE_NOAUTOCLOSE
!define MUI_ABORTWARNING # This will warn the user if he exits from the installer.
!define MUI_FINISHPAGE_NOREBOOTSUPPORT
!define MUI_FINISHPAGE_LINK "Gameplay manual at Allure of the Stars website"
!define MUI_FINISHPAGE_LINK_LOCATION https://github.com/AllureOfTheStars/Allure/blob/master/GameDefinition/PLAYING.md
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "Games"
!define MUI_STARTMENUPAGE_TEXT_CHECKBOX "Do not create start menu nor desktop shortcuts"

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_DIRECTORY

  ;Start Menu Folder Page Configuration
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM"
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\Allure"
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

  !insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Dummy Section" SecDummy

#SetShellVarContext all

  SetOutPath "$INSTDIR"

File favicon.ico

!include WinVer.nsh
${If} ${AtLeastWinVista}

  ${If} ${RunningX64}
    File /r AllureOfTheStars64\*
  ${Else}
    File /r AllureOfTheStars32\*
  ${Endif}

${Else}
  MessageBox MB_OK|MB_ICONSTOP "This program requires Windows Vista or newer."
  Quit
${Endif}

  ;Store installation folder
  WriteRegStr HKLM "Software\Allure" "Install_Dir" $INSTDIR

  WriteUninstaller "$INSTDIR\Uninstall.exe"
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Allure" "DisplayName" "Allure"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Allure" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Allure" "DisplayIcon" "$INSTDIR\favicon.ico"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Allure" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Allure" "NoRepair" 1

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application

    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
    CreateShortcut "$SMPROGRAMS\$StartMenuFolder\Uninstall Allure.lnk" "$INSTDIR\Uninstall.exe"
    CreateShortcut "$SMPROGRAMS\$StartMenuFolder\Allure.lnk" "$INSTDIR\Allure.exe" "" "$INSTDIR\favicon.ico"

    CreateShortcut "$DESKTOP\Allure.lnk" "$INSTDIR\Allure.exe" "" "$INSTDIR\favicon.ico"

  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  RMDir /r "$INSTDIR"

  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder

  Delete "$SMPROGRAMS\$StartMenuFolder\Uninstall Allure.lnk"
  Delete "$SMPROGRAMS\$StartMenuFolder\Allure.lnk"
  RMDir "$SMPROGRAMS\$StartMenuFolder"
  Delete "$DESKTOP\Allure.lnk"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Allure"
  DeleteRegKey /ifempty HKLM "Software\Allure"

SectionEnd
