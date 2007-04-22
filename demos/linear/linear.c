/*	$Csoft: glview.c,v 1.5 2005/10/08 02:45:25 vedge Exp $	*/
/*	Public domain	*/

/*
 * This program uses Agar-SG to display a 3D plot of (a subset of) all
 * linear combinations of 3 arbitrary vectors.
 */

#include <agar/core.h>
#include <agar/gui.h>
#include <agar/sg.h>
#include <agar/sg/sg_view.h>
#include <agar/sg/sg_gui.h>

#include <string.h>
#include <unistd.h>
#include <math.h>
#include <stdlib.h>

static SG_Real step = 0.25;
static SG_Real c1 = -5.0, c2 = 5.0;
static SG_Real d1 = -5.0, d2 = 5.0;
static SG_Real e1 = -5.0, e2 = 5.0;
static SG_Vector u = { 1.0, 0.0, 0.0 };
static SG_Vector v = { 0.0, 1.0, 0.0 };
static SG_Vector w = { 0.0, 0.0, 1.0 };

static void
LinearPlot(SG *sg, void *p)
{
	SG_Node *node = p;
	SG_Real c, d, e;
	SG_Vector up, vp, wp;

	SG_Begin(SG_POINTS);
	for (c = c1; c < c2; c += step) {
		up = SG_VectorScale(u, c);
		SG_Vertex3v(&up);
		SG_Color3f(c/c2, d/d2, e/e2);
		for (d = d1; d < d2; d += step) {
			vp = SG_VectorScale(v, d);
			vp = SG_VectorAdd(vp, up);
			SG_Color3f(c/c2, d/d2, e/e2);
			SG_Vertex3v(&vp);
			for (e = e1; e < e2; e += step) {
				wp = SG_VectorScale(w, e);
				wp = SG_VectorAdd(wp, up);
				wp = SG_VectorAdd(wp, vp);
				SG_Color3f(c/c2, d/d2, e/e2);
				SG_Vertex3v(&wp);
			}
		}
	}
	SG_End();
}

const SG_NodeOps LinearOps = {
	"Linear plot",
	sizeof(SG_Node),
	0,
	NULL,		/* init */
	NULL,		/* destroy */
	NULL,		/* load */
	NULL,		/* save */
	NULL,		/* edit */
	NULL,		/* menuInstance */
	NULL,		/* menuClass */
	LinearPlot
};

AG_Window *
SettingsWin(void)
{
	AG_Window *win;
	AG_FSpinbutton *fsb;
	AG_Spinbutton *sb;
	AG_HBox *hb;
	
	win = AG_WindowNew(0);
	AG_WindowSetCaption(win, "Parameters");
	AG_WindowSetPosition(win, AG_WINDOW_MIDDLE_LEFT, 0);

	hb = AG_HBoxNew(win, AG_HBOX_HOMOGENOUS);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "c1:");
	SG_WidgetBindReal(fsb, "value", &c1);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "c2:");
	SG_WidgetBindReal(fsb, "value", &c2);
	
	hb = AG_HBoxNew(win, AG_HBOX_HOMOGENOUS);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "d1:");
	SG_WidgetBindReal(fsb, "value", &d1);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "d2:");
	SG_WidgetBindReal(fsb, "value", &d2);
	
	hb = AG_HBoxNew(win, AG_HBOX_HOMOGENOUS);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "e1:");
	SG_WidgetBindReal(fsb, "value", &e1);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "e2:");
	SG_WidgetBindReal(fsb, "value", &e2);

	hb = AG_HBoxNew(win, AG_HBOX_HOMOGENOUS);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "u1:");
	SG_WidgetBindReal(fsb, "value", &u.x);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "u2:");
	SG_WidgetBindReal(fsb, "value", &u.y);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "u3:");
	SG_WidgetBindReal(fsb, "value", &u.z);
	
	hb = AG_HBoxNew(win, AG_HBOX_HOMOGENOUS);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "v1:");
	SG_WidgetBindReal(fsb, "value", &v.x);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "v2:");
	SG_WidgetBindReal(fsb, "value", &v.y);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "v3:");
	SG_WidgetBindReal(fsb, "value", &v.z);
	
	hb = AG_HBoxNew(win, AG_HBOX_HOMOGENOUS);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "w1:");
	SG_WidgetBindReal(fsb, "value", &w.x);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "w2:");
	SG_WidgetBindReal(fsb, "value", &w.y);
	fsb = AG_FSpinbuttonNew(hb, 0, NULL, "w3:");
	SG_WidgetBindReal(fsb, "value", &w.z);
	
	fsb = AG_FSpinbuttonNew(win, 0, NULL, "step:");
	AG_FSpinbuttonSetIncrement(fsb, 0.01);
	SG_WidgetBindReal(fsb, "value", &step);
	
	AG_WindowShow(win);
}

int
main(int argc, char *argv[])
{
	int c, i, fps = -1;
	AG_Window *win;
	SG_View *sv;
	SG_Node *node;
	SG *sg;
	char *s;

	if (AG_InitCore("linear-demo", 0) == -1) {
		fprintf(stderr, "%s\n", AG_GetError());
		return (1);
	}
	if (AG_InitVideo(800, 600, 32, AG_VIDEO_OPENGL) == -1 ||
	    AG_InitInput(0) == -1) {
		fprintf(stderr, "%s\n", AG_GetError());
		return (-1);
	}
	AG_InitConfigWin(0);
	AG_SetRefreshRate(fps);
	AG_BindGlobalKey(SDLK_ESCAPE, KMOD_NONE, AG_Quit);
	AG_BindGlobalKey(SDLK_F8, KMOD_NONE, AG_ViewCapture);
	agColors[WINDOW_BG_COLOR] = SDL_MapRGB(agVideoFmt, 0,0,0);
	SettingsWin();

	sg = SG_New(agWorld, "scene");
	node = SG_NodeAdd(sg->root, "linear", &LinearOps, 0);
	SG_Translate3(node, 0.0, 10.0, 0.0);

	win = AG_WindowNew(0);
	AG_WindowSetCaption(win, "Linear combinations");
	sv = SG_ViewNew(win, sg, SG_VIEW_EXPAND);
	AG_WindowSetGeometry(win, 196, 0, agView->w-196, agView->h);
	AG_WindowShow(win);

	AG_EventLoop();
	AG_Destroy();
	return (0);
fail:
	AG_Destroy();
	return (1);
}
