/*	$Csoft$	*/
/*	Public domain	*/

#ifndef _SG_GUI_H_
#define _SG_GUI_H_
#include <agar/gui/gui.h>

#ifdef SG_DOUBLE_PRECISION
#define SG_WidgetBindReal(w,n,p) AG_WidgetBind((w),(n),AG_WIDGET_DOUBLE,(p))
#else
#define SG_WidgetBindReal(w,n,p) AG_WidgetBind((w),(n),AG_WIDGET_FLOAT,(p))
#endif

__BEGIN_DECLS
AG_FSpinbutton	*SG_SpinReal(void *, const char *, SG_Real *);
AG_FSpinbutton	*SG_SpinRealInc(void *, const char *, SG_Real *, SG_Real);
AG_FSpinbutton	*SG_SpinFloat(void *, const char *, float *);
AG_FSpinbutton	*SG_SpinDouble(void *, const char *, double *);
AG_Spinbutton	*SG_SpinInt(void *, const char *, int *);

void	*SG_EditVector3(void *, const char *, SG_Vector *);
void	*SG_EditVector4(void *, const char *, SG_Vector4 *);
void	*SG_EditMatrix(void *, const char *, SG_Matrix *);
void	*SG_EditTranslate3(void *, const char *, SG_Matrix *);
void	*SG_EditTranslate4(void *, const char *, SG_Matrix *);
void	*SG_EditScale3(void *, const char *, SG_Matrix *);
void	*SG_EditScale4(void *, const char *, SG_Matrix *);
__END_DECLS
#endif /* _SG_GUI_H_ */