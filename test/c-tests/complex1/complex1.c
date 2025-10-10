/* A code without loops, but with a lot of branches
   Function under test has no arguments, which
   shows the need to consider global variables as symbolic inputs
*/
#include <stdlib.h>

typedef enum {A, B} Letter;
typedef enum {Up, Down} UpDown;
typedef enum {UNDEFINED, Rung, Tel} Base;
typedef enum {Unpaged, Paged} TOM;

const Letter THIS_LETTER = A;

UpDown SO_FD,SO_Domes,Son_FD,Domes,FD_Dec,Transfer_Dec,TO_Page_Dec,TO_Noc_Dec,PDG_Dec,VS_Dec,SYNC_Dec;

Base SO_ROC,SO_PDG,SO_CHIP,SO_VS,ROC,PDG,CHIP,VS;

TOM TO;

Letter FP;

int In_TO_Paged,Son_FD_Up,OK_Vit,Trigger,In_ROC_Tel,In_ROC_Vit,In_PDG_Tel,In_PDG_Vit,Z_PDG_Vital,In_CHIP_Tel,In_CHIP_Vit,In_VS_Tel,In_VS_Vit,Z_VS_Vital,SO_FD_Up,SO_Domes_Up,SO_Roll_Tel,SO_Hdg_Tel,SO_CHIP_Tel,SO_Vs_Tel,Des,pre_Des,SOV_Running;

void Complex1() {
  int Z_Page_TO,Z_Sid_TO,Z_TO_Paged,Des_Condition,Z_FD_Dec_Fo,Z_FD_Dec_Fo_Reg,Empty_FD_Dec_Fo,Z_Transfer_Dec_Fo,Z_Transfer_Dec_Fo_Reg,Empty_Transfer_Dec_Fo,Z_TO_Page_Dec_Fo,Z_TO_Page_Dec_Fo_Reg,Empty_TO_Page_Dec_Fo,Z_TO_Noc_Dec_Fo,Z_TO_Noc_Dec_Fo_Reg,Empty_TO_Noc_Dec_Fo,Z_PDG_Dec_Fo,Z_PDG_Dec_Fo_Reg,Empty_PDG_Dec_Fo,Z_SOV_Stopped,Z_SOV_Stopped_Reg,Empty_SOV_Stopped,Z_VS_Dec_Fo,Z_VS_Dec_Fo_Reg,Empty_VS_Dec_Fo,Z_SYNC_Dec_Fo,Z_SYNC_Dec_Fo_Reg,Empty_SYNC_Dec_Fo,Z_FP_Transfer,Z_Turn_FD_Up,Z_Turn_FD_Down,Z_Thin_Dome_Lay_Tel,Z_Lace_Dome_Lay_Tel,Z_Turn_Domes_Up,Z_Turn_Domes_Down,Z_Ex_Thin_Dome_Vital,In_No_Ex_Thin_Dome_Vit,Do_ROC,Undo_ROC,Do_PDG,Undo_PDG,Z_Ex_Lace_Dome_Vital,In_No_Ex_Lace_Dome_Vit,Do_CHIP,Undo_CHIP,Do_VS,Undo_VS,Z_Domes_Down;
  TOM pre_TO;
  Letter pre_FP;
  UpDown pre_Domes;
  if(SO_FD_Up){SO_FD = Up;}else{SO_FD = Down;}
  if(SO_Domes_Up){SO_Domes = Up;}else{SO_Domes = Down;}
  if(SO_Roll_Tel){SO_ROC = Tel;}else{SO_ROC = Rung;}
  if(SO_Hdg_Tel){SO_PDG = Tel;}else{SO_PDG = Rung;}
  if(SO_CHIP_Tel){SO_CHIP = Tel;}else{SO_CHIP = Rung;}
  if(SO_Vs_Tel){SO_VS = Tel;}else{SO_VS = Rung;}
  Des_Condition = Des;
  Z_SYNC_Dec_Fo = SYNC_Dec==Up;
  Empty_SYNC_Dec_Fo = 1;
  Z_SYNC_Dec_Fo_Reg = Z_SYNC_Dec_Fo && Empty_SYNC_Dec_Fo;
  Z_VS_Dec_Fo = VS_Dec==Up;
  Empty_VS_Dec_Fo = 1;
  Z_VS_Dec_Fo_Reg = Z_VS_Dec_Fo && Empty_VS_Dec_Fo;
  Z_SOV_Stopped = SOV_Running;
  Empty_SOV_Stopped = !Z_VS_Dec_Fo && Empty_VS_Dec_Fo;
  Z_SOV_Stopped_Reg = Z_SOV_Stopped && Empty_SOV_Stopped;
  Z_PDG_Dec_Fo = PDG_Dec==Up;
  Empty_PDG_Dec_Fo = 1;
  Z_PDG_Dec_Fo_Reg = Z_PDG_Dec_Fo && Empty_PDG_Dec_Fo;
  Z_TO_Noc_Dec_Fo =  TO_Noc_Dec==Up;
  Empty_TO_Noc_Dec_Fo = 1;
  Z_TO_Noc_Dec_Fo_Reg = Z_TO_Noc_Dec_Fo && Empty_TO_Noc_Dec_Fo;
  Z_TO_Page_Dec_Fo =  TO_Page_Dec==Up;
  Empty_TO_Page_Dec_Fo = !Z_TO_Noc_Dec_Fo && Empty_TO_Noc_Dec_Fo;
  Z_TO_Page_Dec_Fo_Reg = Z_TO_Page_Dec_Fo && Empty_TO_Page_Dec_Fo;
  Z_Transfer_Dec_Fo =  Transfer_Dec==Up;
  Empty_Transfer_Dec_Fo = !Z_TO_Page_Dec_Fo && Empty_TO_Page_Dec_Fo && !Z_SOV_Stopped && Empty_SOV_Stopped;
  Z_Transfer_Dec_Fo_Reg = Z_Transfer_Dec_Fo && Empty_Transfer_Dec_Fo;
  Z_FD_Dec_Fo = FD_Dec==Up;
  Empty_FD_Dec_Fo = !Z_Transfer_Dec_Fo && Empty_Transfer_Dec_Fo;
  Z_FD_Dec_Fo_Reg = Z_FD_Dec_Fo && Empty_FD_Dec_Fo;
  Z_Page_TO = Z_TO_Page_Dec_Fo_Reg;
  Z_Sid_TO =  Z_TO_Page_Dec_Fo_Reg || Z_TO_Noc_Dec_Fo_Reg;
  pre_TO = TO;
  if(TO==Unpaged){if(Z_Page_TO){TO = Paged;}}else {if(Z_Sid_TO){TO = Unpaged;}}
  In_TO_Paged = TO==Paged;
  Z_TO_Paged = TO==Paged && pre_TO==Unpaged;
  pre_FP = FP;
  if(Z_Transfer_Dec_Fo_Reg){if(FP==A){FP=B;}else {FP=A;}}
  Z_FP_Transfer = FP!=pre_FP;
  if((FP==A) ||!(FP==A) && !(SO_Domes==Up)){OK_Vit = 1;} 
  if(!(FP==A) && (SO_Domes==Up)){OK_Vit = 0;}
  Z_Thin_Dome_Lay_Tel = (!Trigger || OK_Vit) && Z_PDG_Dec_Fo_Reg;
  Z_Lace_Dome_Lay_Tel = (!Trigger || OK_Vit) && Z_VS_Dec_Fo_Reg || (OK_Vit && Z_SOV_Stopped_Reg && !In_VS_Vit && !pre_Des);
  Z_Turn_FD_Up = Z_FD_Dec_Fo_Reg || Z_TO_Paged || Des_Condition || Z_Thin_Dome_Lay_Tel || Z_Lace_Dome_Lay_Tel || (Z_FP_Transfer &&  FP==THIS_LETTER &&  Trigger);
  Z_Turn_FD_Down =  Z_FD_Dec_Fo_Reg && !Des_Condition;
  if(Son_FD==Down) {if(Z_Turn_FD_Up) {Son_FD = Up;}}else{if(Z_Turn_FD_Down) {Son_FD = Down;}}
  Son_FD_Up = (Son_FD==Up);
  Z_Turn_Domes_Up = Son_FD==Up || SO_FD==Up || In_TO_Paged;
  Z_Turn_Domes_Down = Son_FD==Down && SO_FD==Down && TO==Unpaged;
  pre_Domes = Domes;
  if(OK_Vit && Z_Turn_Domes_Up) {Domes = Up;}
  if(OK_Vit && Z_Turn_Domes_Down) {Domes = Down;}
  Trigger = Domes==Up;
  Z_Domes_Down = (Domes==Down) && (pre_Domes==Up);
  Do_PDG = Z_PDG_Dec_Fo_Reg;
  Z_PDG_Vital = Do_PDG && !(PDG==Tel) && OK_Vit;
  Z_PDG_Vital = Do_PDG;
  Z_Ex_Thin_Dome_Vital =  Z_PDG_Vital;
  Do_VS =  Z_VS_Dec_Fo_Reg && !Des_Condition;
  Z_VS_Vital = Do_VS && !(VS==Tel) && OK_Vit;
  Z_Ex_Lace_Dome_Vital = Z_VS_Vital;
  if(!Z_PDG_Dec_Fo_Reg && Z_Ex_Thin_Dome_Vital)
    return; // unreachable
  Undo_PDG =  Z_PDG_Dec_Fo_Reg || Z_Ex_Thin_Dome_Vital || Z_FP_Transfer || Z_Domes_Down;
  if(OK_Vit){if(PDG==UNDEFINED){if(Do_PDG){PDG = Tel;}else{PDG = Rung;}}else if(PDG==Tel){if(Undo_PDG){PDG = Rung;}}else if(PDG==Rung){if(Do_PDG){PDG = Tel;}}}else{PDG = SO_PDG;}
  In_PDG_Tel = (PDG==Tel);
  In_PDG_Vit = (PDG==Tel);
  In_No_Ex_Thin_Dome_Vit =  !In_PDG_Vit;
  Do_ROC = In_No_Ex_Thin_Dome_Vit && (Domes==Up);
  Undo_ROC = Z_Ex_Thin_Dome_Vital || Z_Domes_Down;
  if(OK_Vit){if(ROC==UNDEFINED){if(Do_ROC){ROC = Tel;}else{ROC = Rung;}}else if(ROC==Tel){if(Undo_ROC){ROC = Rung;}}else if(ROC==Rung){if(Do_ROC){ROC = Tel;}}}else{ROC = SO_ROC;}
  In_ROC_Tel = (ROC==Tel);
  In_ROC_Vit = (ROC==Tel);
  if(!Z_VS_Dec_Fo_Reg && Z_Ex_Lace_Dome_Vital)
    return; // unreachable
  Undo_VS =  Z_VS_Dec_Fo_Reg || Z_Ex_Lace_Dome_Vital || Z_FP_Transfer || Z_Domes_Down;
  if(OK_Vit){if(VS==UNDEFINED){if(Do_VS){VS = Tel;}else{VS = Rung;}}else if(VS==Tel){if(Undo_VS){VS = Rung;}}else if(VS==Rung){if(Do_VS){VS = Tel;}}}else{VS = SO_VS;}
  In_VS_Tel = (VS==Tel);
  In_VS_Vit = (VS==Tel);
  In_No_Ex_Lace_Dome_Vit = !In_VS_Vit;
  Do_CHIP = In_No_Ex_Lace_Dome_Vit && (Domes==Up);
  Undo_CHIP = Z_Ex_Lace_Dome_Vital || Z_Domes_Down;
  if(OK_Vit){if(CHIP==UNDEFINED){if(Do_CHIP){CHIP = Tel;}else{CHIP = Rung;}}else if(CHIP==Tel){if(Undo_CHIP){CHIP = Rung;}}else if(CHIP==Rung){if(Do_CHIP){CHIP = Tel;}}}else{CHIP = SO_CHIP;}
  In_CHIP_Tel = (CHIP==Tel);
  In_CHIP_Vit = (CHIP==Tel);
  pre_Des = Des;
  return;
}
