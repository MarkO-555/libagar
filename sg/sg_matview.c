/*
 * Copyright (c) 2005-2007 Hypertriton, Inc.
 * <http://www.hypertriton.com/>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <agar/core/core.h>
#include <agar/core/view.h>

#include "sg.h"
#include "sg_matview.h"

#include <agar/gui/window.h>
#include <agar/gui/primitive.h>
#include <agar/gui/label.h>

#include <stdarg.h>
#include <string.h>
#include <errno.h>

static AG_WidgetOps agMatviewOps = {
	{
		"AG_Widget:AG_Matview",
		sizeof(AG_Matview),
		{ 0,0 },
		NULL,			/* init */
		NULL,			/* reinit */
		NULL,			/* destroy */
		NULL,			/* load */
		NULL,			/* save */
		NULL			/* edit */
	},
	AG_MatviewDrawNumerical,
	AG_MatviewScale
};

AG_Matview *
AG_MatviewNew(void *parent, SG_Matrix *mat, Uint flags)
{
	AG_Matview *mv;

	mv = Malloc(sizeof(AG_Matview), M_OBJECT);
	AG_MatviewInit(mv, mat, flags);
	AG_ObjectAttach(parent, mv);
	return (mv);
}

static void
matview_keydown(AG_Event *event)
{
	AG_Matview *mv = AG_SELF();
	int keysym = AG_INT(1);

	switch (keysym) {
	case SDLK_g:
		AGWIDGET_OPS(mv)->draw = AG_MatviewDrawGreyscale;
		break;
	case SDLK_n:
		AGWIDGET_OPS(mv)->draw = AG_MatviewDrawNumerical;
		break;
	case SDLK_EQUALS:
		mv->scale++;
		break;
	case SDLK_MINUS:
		if (mv->scale-1 >= 0) {
			mv->scale--;
		}
		break;
	}
}

static void
matview_mousebuttondown(AG_Event *event)
{
	AG_Button *bu = AG_SELF();

	AG_WidgetFocus(bu);
}

void
AG_MatviewInit(AG_Matview *mv, SG_Matrix *mat, Uint flags)
{
	static int max_m = 4, max_n = 4;

	AG_WidgetInit(mv, "matview", &agMatviewOps,
	    AG_WIDGET_HFILL|AG_WIDGET_VFILL|AG_WIDGET_CLIPPING|
	    AG_WIDGET_FOCUSABLE);
	mv->mat = mat;
	mv->flags = flags;
	mv->hspace = 2;
	mv->vspace = 2;
	mv->hbar = AG_ScrollbarNew(mv, AG_SCROLLBAR_HORIZ, 0);
	mv->vbar = AG_ScrollbarNew(mv, AG_SCROLLBAR_VERT, 0);
	mv->xoffs = 0;
	mv->yoffs = 0;
	mv->scale = 10;
	mv->pre_m = 4;
	mv->pre_n = 4;
	mv->numfmt = "%g";
	
	AG_WidgetBind(mv->hbar, "value", AG_WIDGET_INT, &mv->xoffs);
	AG_WidgetBind(mv->vbar, "value", AG_WIDGET_INT, &mv->yoffs);
	AG_WidgetBind(mv->hbar, "max", AG_WIDGET_INT, &max_m);
	AG_WidgetBind(mv->vbar, "max", AG_WIDGET_INT, &max_n);
	AG_WidgetSetInt(mv->hbar, "min", 0);
	AG_WidgetSetInt(mv->vbar, "min", 0);

	AG_TextPrescale("-00", &mv->ent_w, &mv->ent_h);
	AG_SetEvent(mv, "window-keydown", matview_keydown, NULL);
	AG_SetEvent(mv, "window-mousebuttondown", matview_mousebuttondown,
	    NULL);
}

void
AG_MatviewSetNumericalFmt(AG_Matview *mv, const char *fmt)
{
	mv->numfmt = fmt;
}

void
AG_MatviewPrescale(AG_Matview *mv, const char *text, Uint m, Uint n)
{
	mv->pre_m = m;
	mv->pre_n = n;
	AG_TextPrescale(text, &mv->ent_w, &mv->ent_h);
}

void
AG_MatviewScale(void *p, int w, int h)
{
	AG_Matview *mv = p;

	if (w == -1 && h == -1) {
		AGWIDGET(mv)->w = mv->pre_n*(mv->ent_w + mv->hspace) +
		    mv->hspace*2;
		AGWIDGET(mv)->h = mv->pre_m*(mv->ent_h + mv->vspace) +
		    mv->vspace*2;
		return;
	}

	AGWIDGET(mv->hbar)->x = 0;
	AGWIDGET(mv->hbar)->y = AGWIDGET(mv)->h - mv->hbar->bw;
	AGWIDGET(mv->hbar)->w = AGWIDGET(mv)->w;
	AGWIDGET(mv->hbar)->h = mv->hbar->bw;

	AGWIDGET(mv->vbar)->x = AGWIDGET(mv)->w - mv->vbar->bw;
	AGWIDGET(mv->vbar)->y = mv->vbar->bw;
	AGWIDGET(mv->vbar)->w = mv->vbar->bw;
	AGWIDGET(mv->vbar)->h = AGWIDGET(mv)->h - mv->vbar->bw;
}

void
AG_MatviewDrawNumerical(void *p)
{
	char text[8];
	AG_Matview *mv = p;
	SG_Matrix *M = mv->mat;
	int m, n;
	SDL_Surface *su;
	int x, y;

	agPrim.box(mv, 0, 0, AGWIDGET(mv)->w, AGWIDGET(mv)->h, -1,
	    AG_COLOR(BG_COLOR));

	for (m = -1, y = -mv->yoffs*mv->ent_h;
	     m < 4 && y < AGWIDGET(mv)->h;
	     m++, y += (mv->ent_h + mv->vspace)) {
		for (n = -1, x = -mv->xoffs*mv->ent_w;
		     n < 4 && x < AGWIDGET(mv)->w;
		     n++, x += (mv->ent_w + mv->hspace)) {
			if (m == -1) {
				snprintf(text, sizeof(text), "%d", n);
			} else if (n == -1) {
				snprintf(text, sizeof(text), "%d", m);
			} else {
				agPrim.box(mv, x, y,
				    mv->ent_w, mv->ent_h, -1,
				    AG_COLOR(FRAME_COLOR));
				snprintf(text, sizeof(text), mv->numfmt,
				    M->m[m][n]);
			}
			su = AG_TextRender(NULL, -1, AG_COLOR(TEXT_COLOR),
			    text);
			AG_WidgetBlit(mv, su, x, y);
			SDL_FreeSurface(su);
		}
	}
}

void
AG_MatviewDrawGreyscale(void *p)
{
	AG_Matview *mv = p;
	SG_Matrix *A = mv->mat;
	SDL_Surface *su;
	Uint m, n;
	int x, y;
	SG_Real big = 0.0, small = 0.0;

	agPrim.box(mv, 0, 0, AGWIDGET(mv)->w, AGWIDGET(mv)->h, -1,
	    AG_COLOR(BG_COLOR));

	for (m = 0; m < 4; m++) {
		for (n = 0; n < 4; n++) {
			if (A->m[m][n] > big) { big = A->m[m][n]; }
			if (A->m[m][n] < small) { small = A->m[m][n]; }
		}
	}

	for (m = 0, y = -mv->yoffs*mv->scale;
	     m < 4 && y < AGWIDGET(mv)->h;
	     m++, y += mv->scale) {
		for (n = 0, x = -mv->xoffs*mv->scale;
		     n < 4 && x < AGWIDGET(mv)->w;
		     n++, x += mv->scale) {
		     	SG_Real dv = A->m[m][n];
			SDL_Rect rd;
			Uint32 c;
			Uint8 v;
			Uint vi;

			if (dv == HUGE_VAL) {
				c = SDL_MapRGB(agVideoFmt, 200, 0, 0);
			} else {
				if (dv >= 0.0) {
					v = 128 + (Uint8)(dv*128.0/big);
					c = SDL_MapRGB(agVideoFmt, v, 0, 0);
				} else {
					v = 128 + (Uint8)((1.0/dv)*128.0 /
					    (1.0/small));
					c = SDL_MapRGB(agVideoFmt, 0, 0, v);
				}
			}
			if (dv != 0.0) {
				rd.x = AGWIDGET(mv)->cx+x;
				rd.y = AGWIDGET(mv)->cy+y;
				rd.w = mv->scale;
				rd.h = mv->scale;
				SDL_FillRect(agView->v, &rd, c);
			}
		}
	}
}
