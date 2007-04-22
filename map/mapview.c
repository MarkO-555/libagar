/*	$Csoft: mapview.c,v 1.50 2005/10/02 09:39:55 vedge Exp $	*/

/*
 * Copyright (c) 2002, 2003, 2004, 2005 CubeSoft Communications, Inc.
 * <http://www.csoft.org>
 * All rights reserved.
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

#include <agar/gui/window.h>
#include <agar/gui/primitive.h>
#include <agar/gui/toolbar.h>
#include <agar/gui/statusbar.h>
#include <agar/gui/scrollbar.h>

#include "map.h"
#include "mapview.h"
#include "nodesel.h"
#include "refsel.h"
#include "tools.h"

#include <stdarg.h>
#include <string.h>

const AG_WidgetOps mapViewOps = {
	{
		"AG_Widget:MAP_View",
		sizeof(MAP_View),
		{ 0,0 },
		NULL,		/* init */
		NULL,		/* reinit */
		MAP_ViewDestroy,
		NULL,		/* load */
		NULL,		/* save */
		NULL		/* edit */
	},
	MAP_ViewDraw,
	MAP_ViewScale
};

enum {
	ZOOM_MIN =	 20,	/* Min zoom factor (%) */
	ZOOM_MAX =	 500,	/* Max zoom factor (%) */
	ZOOM_PROPS_MIN = 60,	/* Min zoom factor for showing properties (%) */
	ZOOM_GRID_MIN =	 20	/* Min zoom factor for showing the grid (%) */
};

int	agMapviewBg = 1;		/* Background tiles enable */
int	agMapviewAnimatedBg = 1;	/* Background tiles moving */
int	agMapviewBgTileSize = 8;	/* Background tile size */
int	agMapviewEditSelOnly = 0;	/* Restrict edition to selection */
int	agMapviewZoomInc = 8;


static void lost_focus(AG_Event *);
static void mousemotion(AG_Event *);
static void mousebuttondown(AG_Event *);
static void mousebuttonup(AG_Event *);
static void key_up(AG_Event *);
static void key_down(AG_Event *);

MAP_View *
MAP_ViewNew(void *parent, MAP *m, int flags, struct ag_toolbar *toolbar,
    struct ag_statusbar *statbar)
{
	MAP_View *mv;

	mv = Malloc(sizeof(MAP_View), M_OBJECT);
	MAP_ViewInit(mv, m, flags, toolbar, statbar);
	AG_ObjectAttach(parent, mv);
	return (mv);
}

void
MAP_ViewPixel2i(MAP_View *mv, int x, int y)
{
	Uint8 r, g, b;
	int dx = AGWIDGET(mv)->cx + x;
	int dy = AGWIDGET(mv)->cy + y;

	if (mv->col.a < 255) {
		AG_BLEND_RGBA2_CLIPPED(agView->v, dx, dy,
		    mv->col.r, mv->col.g, mv->col.b, mv->col.a,
		    AG_ALPHA_OVERLAY);
	} else {
		AG_VIEW_PUT_PIXEL2_CLIPPED(dx, dy, mv->col.pixval);
	}
}

void
MAP_ViewHLine(MAP_View *mv, int x1, int x2, int y)
{
	int x;

	/* TODO opengl */
	if (!agView->opengl) {
		for (x = x1; x < x2; x++)
			MAP_ViewPixel2i(mv, x, y);
	}
}

void
MAP_ViewVLine(MAP_View *mv, int x, int y1, int y2)
{
	int y;

	/* TODO opengl */
	if (!agView->opengl) {
		for (y = y1; y < y2; y++)
			MAP_ViewPixel2i(mv, x, y);
	}
}

void
MAP_ViewControl(MAP_View *mv, const char *slot, void *obj)
{
#ifdef DEBUG
	if (!AG_ObjectIsClass(obj, "MAP_Actor:*"))
		fatal("%s: not an actor", AGOBJECT(obj)->name);
#endif
	mv->actor = (MAP_Actor *)obj;
}

#ifdef EDITION

void
MAP_ViewSelectTool(MAP_View *mv, MAP_Tool *ntool, void *p)
{
	AG_Window *pwin;

	if (mv->curtool != NULL) {
		if (mv->curtool->trigger != NULL) {
			AG_WidgetSetBool(mv->curtool->trigger, "state", 0);
		}
		if (mv->curtool->win != NULL) {
			AG_WindowHide(mv->curtool->win);
		}
		if (mv->curtool->pane != NULL) {
			AG_Widget *wt;
			AG_Window *pwin;

			AGOBJECT_FOREACH_CHILD(wt, mv->curtool->pane,
			    ag_widget) {
				AG_ObjectDetach(wt);
				AG_ObjectDestroy(wt);
				Free(wt, M_OBJECT);
			}
			if ((pwin = AG_WidgetParentWindow(mv->curtool->pane))
			    != NULL) {
				AG_WINDOW_UPDATE(pwin);
			}
		}
		mv->curtool->mv = NULL;

		AG_WidgetReplaceSurface(mv->status, mv->status->surface,
		    AG_TextRender(NULL, -1, AG_COLOR(TEXT_COLOR),
		    _("Select a tool or double-click on an element to insert.")
		    ));
	}
	mv->curtool = ntool;

	if (ntool != NULL) {
		ntool->p = p;
		ntool->mv = mv;

		if (ntool->trigger != NULL) {
			AG_WidgetSetBool(ntool->trigger, "state", 1);
		}
		if (ntool->win != NULL) {
			AG_WindowShow(ntool->win);
		}
		if (ntool->pane != NULL && ntool->ops->edit_pane != NULL) {
			AG_Window *pwin;

			ntool->ops->edit_pane(ntool, ntool->pane);
			if ((pwin = AG_WidgetParentWindow(mv->curtool->pane))
			    != NULL) {
				AG_WINDOW_UPDATE(pwin);
			}
		}
		MAP_ToolUpdateStatus(ntool);
	}

	if ((pwin = AG_WidgetParentWindow(mv)) != NULL) {
		agView->focus_win = pwin;
		AG_WidgetFocus(mv);
	}
}

MAP_Tool *
MAP_ViewFindTool(MAP_View *mv, const char *name)
{
	MAP_Tool *tool;

	TAILQ_FOREACH(tool, &mv->tools, tools) {
		if (strcmp(tool->ops->name, name) == 0)
			return (tool);
	}
	return (NULL);
}

static void
tool_selected_ev(AG_Event *event)
{
	MAP_View *mv = AG_PTR(1);
	MAP_Tool *tool = AG_PTR(2);
	void *p = AG_PTR(3);

	if (mv->curtool == tool) {
		MAP_ViewSelectTool(mv, NULL, NULL);
	} else {
		MAP_ViewSelectTool(mv, tool, p);
	}
}

MAP_Tool *
MAP_ViewRegTool(MAP_View *mv, const MAP_ToolOps *ops, void *p)
{
	MAP_Tool *t;

	t = Malloc(ops->len, M_MAPEDIT);
	t->ops = ops;
	t->mv = mv;
	t->p = p;
	MAP_ToolInit(t);

	if (((ops->flags & TOOL_HIDDEN) == 0) && mv->toolbar != NULL) {
		SDL_Surface *icon = ops->icon >= 0 ? AGICON(ops->icon) : NULL;

		t->trigger = AG_ToolbarButtonIcon(mv->toolbar,
		    icon, 0, tool_selected_ev,
		    "%p, %p, %p", mv, t, p);
#if 0
		AG_SetEvent(t->trigger, "button-mouseoverlap",
		    update_tooltips, "%p, %p", mv, t);
#endif
	}

	TAILQ_INSERT_TAIL(&mv->tools, t, tools);
	return (t);
}

void
MAP_ViewSetDefaultTool(MAP_View *mv, MAP_Tool *tool)
{
	mv->deftool = tool;
}
#endif /* EDITION */

void
MAP_ViewRegDrawCb(MAP_View *mv,
    void (*draw_func)(MAP_View *, void *), void *p)
{
	MAP_ViewDrawCb *dcb;

	dcb = Malloc(sizeof(MAP_ViewDrawCb), M_WIDGET);
	dcb->func = draw_func;
	dcb->p = p;
	SLIST_INSERT_HEAD(&mv->draw_cbs, dcb, draw_cbs);
}

void
MAP_ViewDestroy(void *p)
{
	MAP_View *mv = p;
	MAP_ViewDrawCb *dcb, *ndcb;
	int i;

	for (dcb = SLIST_FIRST(&mv->draw_cbs);
	     dcb != SLIST_END(&mv->draw_cbs);
	     dcb = ndcb) {
		ndcb = SLIST_NEXT(dcb, draw_cbs);
		Free(dcb, M_WIDGET);
	}
	AG_WidgetDestroy(mv);
}

static void
mapview_detached(AG_Event *event)
{
	MAP_View *mv = AG_SELF();
	MAP_Tool *tool, *ntool;

	for (tool = TAILQ_FIRST(&mv->tools);
	     tool != TAILQ_END(&mv->tools);
	     tool = ntool) {
		ntool = TAILQ_NEXT(tool, tools);
		MAP_ToolDestroy(tool);
		Free(tool, M_MAPEDIT);
	}
	TAILQ_INIT(&mv->tools);
	mv->curtool = NULL;
}

static void
dblclick_expired(AG_Event *event)
{
	MAP_View *mv = AG_SELF();

	mv->dblclicked = 0;
}

static void
scrolled_bar(AG_Event *event)
{
	AG_Scrollbar *sb = AG_SELF();
	MAP_View *mv = AG_PTR(1);

	MAP_ViewUpdateCamera(mv);
}

static void
resized_map(AG_Event *event)
{
	MAP_View *mv = AG_SELF();

	MAP_ViewUpdateCamera(mv);
}

void
MAP_ViewInit(MAP_View *mv, MAP *m, int flags,
    struct ag_toolbar *toolbar, struct ag_statusbar *statbar)
{
	AG_WidgetInit(mv, "mapview", &mapViewOps,
	    AG_WIDGET_FOCUSABLE|AG_WIDGET_CLIPPING|AG_WIDGET_HFILL|
	    AG_WIDGET_VFILL);

	mv->flags = (flags | AG_MAPVIEW_CENTER);
	mv->mode = AG_MAPVIEW_EDITION;
	mv->edit_attr = 0;
	mv->map = m;
	mv->actor = NULL;
	mv->cam = 0;
	mv->mw = 0;					/* Set on scale */
	mv->mh = 0;
	mv->prew = 4;
	mv->preh = 4;
	
	mv->mouse.scrolling = 0;
	mv->mouse.x = 0;
	mv->mouse.y = 0;
	mv->mouse.xmap = 0;
	mv->mouse.ymap = 0;
	mv->mouse.xmap_rel = 0;
	mv->mouse.ymap_rel = 0;

	mv->dblclicked = 0;
	mv->hbar = NULL;
	mv->vbar = NULL;
	mv->toolbar = toolbar;
	mv->statusbar = statbar;
	mv->status = (statbar != NULL) ?
	             AG_StatusbarAddLabel(statbar, AG_LABEL_STATIC, "...") :
		     NULL;
	mv->lib_tl = NULL;
	mv->objs_tl = NULL;
	mv->layers_tl = NULL;
	mv->curtool = NULL;
	mv->deftool = NULL;
	TAILQ_INIT(&mv->tools);
	SLIST_INIT(&mv->draw_cbs);
	
	mv->cx = -1;
	mv->cy = -1;
	mv->cxrel = 0;
	mv->cyrel = 0;
	mv->cxoffs = 0;
	mv->cyoffs = 0;
	mv->xoffs = 0;
	mv->yoffs = 0;
	
	mv->msel.set = 0;
	mv->msel.x = 0;
	mv->msel.y = 0;
	mv->msel.xoffs = 0;
	mv->msel.yoffs = 0;
	mv->esel.set = 0;
	mv->esel.moving = 0;
	mv->esel.x = 0;
	mv->esel.y = 0;
	mv->esel.w = 0;
	mv->esel.h = 0;
	mv->rsel.moving = 0;

	mv->col.r = 255;
	mv->col.g = 255;
	mv->col.b = 255;
	mv->col.a = 32;
	mv->col.pixval = SDL_MapRGB(agVideoFmt, 255, 255, 255);

	AG_MutexLock(&m->lock);
	mv->mx = m->origin.x;
	mv->my = m->origin.y;
	AG_MutexUnlock(&m->lock);

	AG_SetEvent(mv, "widget-lostfocus", lost_focus, NULL);
	AG_SetEvent(mv, "widget-hidden", lost_focus, NULL);
	AG_SetEvent(mv, "window-keyup", key_up, NULL);
	AG_SetEvent(mv, "window-keydown", key_down, NULL);
	AG_SetEvent(mv, "window-mousemotion", mousemotion, NULL);
	AG_SetEvent(mv, "window-mousebuttondown", mousebuttondown, NULL);
	AG_SetEvent(mv, "window-mousebuttonup", mousebuttonup, NULL);
	AG_SetEvent(mv, "detached", mapview_detached, NULL);
	AG_SetEvent(mv, "dblclick-expire", dblclick_expired, NULL);
	AG_SetEvent(mv, "map-resized", resized_map, NULL);
}

void
MAP_ViewUseScrollbars(MAP_View *mv, AG_Scrollbar *hbar,
    AG_Scrollbar *vbar)
{
	mv->hbar = hbar;
	mv->vbar = vbar;

	if (hbar != NULL) {
		AGWIDGET(hbar)->flags &= ~(AG_WIDGET_FOCUSABLE);
		AGWIDGET(hbar)->flags |= AG_WIDGET_UNFOCUSED_MOTION;
		AG_WidgetBind(mv->hbar, "value", AG_WIDGET_INT, &AGMCAM(mv).x);
		AG_SetEvent(mv->hbar, "scrollbar-changed", scrolled_bar, "%p",
		    mv);
	}
	if (vbar != NULL) {
		AGWIDGET(vbar)->flags &= ~(AG_WIDGET_FOCUSABLE);
		AGWIDGET(vbar)->flags |= AG_WIDGET_UNFOCUSED_MOTION;
		AG_WidgetBind(mv->vbar, "value", AG_WIDGET_INT, &AGMCAM(mv).y);
		AG_SetEvent(mv->vbar, "scrollbar-changed", scrolled_bar, "%p",
		    mv);
	}
}

/*
 * Translate widget coordinates to node coordinates.
 * The map must be locked.
 */
static __inline__ void
get_node_coords(MAP_View *mv, int *x, int *y)
{
	*x -= mv->xoffs;
	*y -= mv->yoffs;
	mv->cxoffs = *x % AGMTILESZ(mv);
	mv->cyoffs = *y % AGMTILESZ(mv);

	*x = ((*x) - mv->cxoffs)/AGMTILESZ(mv);
	*y = ((*y) - mv->cyoffs)/AGMTILESZ(mv);

	mv->cx = mv->mx + *x;
	mv->cy = mv->my + *y;

	if (mv->cx < 0 || mv->cx >= mv->map->mapw || mv->cxoffs < 0)
		mv->cx = -1;
	if (mv->cy < 0 || mv->cy >= mv->map->maph || mv->cyoffs < 0)
		mv->cy = -1;
}

static void
draw_cursor(MAP_View *mv)
{
	SDL_Rect rd;
	int msx, msy;

	rd.w = AGMTILESZ(mv);
	rd.h = AGMTILESZ(mv);

	if (mv->msel.set) {
#if 0
		/* XXX */
		AG_MouseGetState(&msx, &msy);
		rd.x = msx;
		rd.y = msy;
		if (!agView->opengl) {
			SDL_BlitSurface(AGICON(SELECT_CURSORBMP), NULL,
			    agView->v, &rd);
		}
#endif
		return;
	}

	rd.x = mv->mouse.x*AGMTILESZ(mv) + mv->xoffs;
	rd.y = mv->mouse.y*AGMTILESZ(mv) + mv->yoffs;

	if (mv->curtool == NULL)
		return;

	if (mv->curtool->ops->cursor != NULL) {
		if (mv->curtool->ops->cursor(mv->curtool, &rd) == -1)
			goto defcurs;
	}
	return;
defcurs:
	agPrim.rect_outlined(mv,
	    rd.x + 1,
	    rd.y + 1,
	    AGMTILESZ(mv) - 1,
	    AGMTILESZ(mv) - 1,
	    AG_COLOR(MAPVIEW_CURSOR_COLOR));
	agPrim.rect_outlined(mv,
	    rd.x + 2,
	    rd.y + 2,
	    AGMTILESZ(mv) - 3,
	    AGMTILESZ(mv) - 3,
	    AG_COLOR(MAPVIEW_CURSOR_COLOR));
}

static __inline__ void
center_to_origin(MAP_View *mv)
{
	AGMCAM(mv).x = mv->map->origin.x*AGMTILESZ(mv) - AGMTILESZ(mv)/2;
	AGMCAM(mv).y = mv->map->origin.y*AGMTILESZ(mv) - AGMTILESZ(mv)/2;
	MAP_ViewUpdateCamera(mv);
}

void
MAP_ViewDraw(void *p)
{
#ifdef EDITION
	extern int agEditMode;
#endif
	MAP_View *mv = p;
	MAP_ViewDrawCb *dcb;
	MAP *m = mv->map;
	MAP_Node *node;
	MAP_Item *nref;
	int mx, my, rx = 0, ry = 0;
	int layer = 0;
	int esel_x = -1, esel_y = -1, esel_w = -1, esel_h = -1;
	int msel_x = -1, msel_y = -1, msel_w = -1, msel_h = -1;
	SDL_Rect rExtent;
#ifdef HAVE_OPENGL
	GLboolean blend_save;
	GLint blend_sfactor;
	GLint blend_dfactor;
	GLfloat texenvmode;
#endif

	if (AGWIDGET(mv)->w < AGTILESZ || AGWIDGET(mv)->h < AGTILESZ)
		return;

	if (AGWIDGET(mv)->flags & AG_WIDGET_FOCUSED)
		agPrim.rect_outlined(mv, 0, 0,
		    AGWIDGET(mv)->w, AGWIDGET(mv)->h,
		    AG_COLOR(FOCUS_COLOR));

	SLIST_FOREACH(dcb, &mv->draw_cbs, draw_cbs)
		dcb->func(mv, dcb->p);
	
	if (mv->flags & AG_MAPVIEW_CENTER) {
		mv->flags &= ~(AG_MAPVIEW_CENTER);
		center_to_origin(mv);
	}

	if ((mv->flags & AG_MAPVIEW_NO_BG) == 0) {
		SDL_Rect rtiling;

		rtiling.x = 0;
		rtiling.y = 0;
		rtiling.w = AGWIDGET(mv)->w;
		rtiling.h = AGWIDGET(mv)->h;
		agPrim.tiling(mv, rtiling, agMapviewBgTileSize, 0,
		    AG_COLOR(MAPVIEW_TILE1_COLOR),
		    AG_COLOR(MAPVIEW_TILE2_COLOR));
	}

#ifdef HAVE_OPENGL
	if (agView->opengl) {
		glGetBooleanv(GL_BLEND, &blend_save);
		glGetIntegerv(GL_BLEND_SRC, &blend_sfactor);
		glGetIntegerv(GL_BLEND_DST, &blend_dfactor);
		glGetTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, &texenvmode);

		glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	}
#endif
	AG_MutexLock(&m->lock);

	if (m->map == NULL)
		goto out;
draw_layer:
	if (!m->layers[layer].visible) {
		goto next_layer;
	}
	for (my = mv->my, ry = mv->yoffs;
	     ((my - mv->my) <= mv->mh) && (my < m->maph);
	     my++, ry += AGMTILESZ(mv)) {

		for (mx = mv->mx, rx = mv->xoffs;
	     	     ((mx - mv->mx) <= mv->mw) && (mx < m->mapw);
		     mx++, rx += AGMTILESZ(mv)) {

			node = &m->map[my][mx];

			TAILQ_FOREACH(nref, &node->nrefs, nrefs) {
				if (nref->layer != layer)
					continue;

				MAP_ItemDraw(m, nref,
				    AGWIDGET(mv)->cx + rx,
				    AGWIDGET(mv)->cy + ry,
				    mv->cam);

#ifdef DEBUG
				if (mv->flags & AG_MAPVIEW_SHOW_OFFSETS) {
					agPrim.line(mv, rx, ry,
					    (rx+nref->r_gfx.xcenter +
					     nref->r_gfx.xmotion -
					     nref->r_gfx.xorigin),
					    (ry+nref->r_gfx.ycenter +
					     nref->r_gfx.ymotion -
					     nref->r_gfx.yorigin),
					     AG_COLOR(MAPVIEW_RSEL_COLOR));
				}
#endif /* DEBUG */
				if ((nref->layer == m->cur_layer) &&
				    (mv->mode == AG_MAPVIEW_EDIT_ATTRS)) {
					Uint8 c[4];

					MAP_ItemAttrColor(mv->edit_attr,
					    (nref->flags & mv->edit_attr), c);
					agPrim.rect_blended(mv, rx, ry,
					    AGMTILESZ(mv), AGMTILESZ(mv), c,
					    AG_ALPHA_OVERLAY);
				}

				if ((nref->flags & AG_NITEM_SELECTED) &&
				    MAP_ItemExtent(m, nref, &rExtent, mv->cam)
				    == 0) {
					agPrim.rect_outlined(mv,
					    rx + rExtent.x - 1,
					    ry + rExtent.y - 1,
					    rExtent.w + 1,
					    rExtent.h + 1,
					    AG_COLOR(MAPVIEW_RSEL_COLOR));
				}
			}
#ifdef EDITION
			if (!agEditMode)
				continue;
				
			if ((mv->flags & AG_MAPVIEW_SHOW_ORIGIN) &&
			    (mx == m->origin.x && my == m->origin.y)) {
				int t2 = AGMTILESZ(mv)/2;
			
				agPrim.circle(mv,
				    rx+t2,
				    ry+t2,
				    t2,
				    AG_COLOR(MAPVIEW_ORIGIN_COLOR));
				agPrim.line(mv,
				    rx+t2, ry,
				    rx+t2, ry+AGMTILESZ(mv),
				    AG_COLOR(MAPVIEW_ORIGIN_COLOR));
				agPrim.line(mv,
				    rx, ry+t2,
				    rx+AGMTILESZ(mv), ry+t2,
				    AG_COLOR(MAPVIEW_ORIGIN_COLOR));
			}
			if (mv->msel.set &&
			    mv->msel.x == mx && mv->msel.y == my) {
				msel_x = rx + 1;
				msel_y = ry + 1;
				msel_w = mv->msel.xoffs*AGMTILESZ(mv) - 2;
				msel_h = mv->msel.yoffs*AGMTILESZ(mv) - 2;
			}
			if (mv->esel.set &&
			    mv->esel.x == mx && mv->esel.y == my) {
				esel_x = rx;
				esel_y = ry;
				esel_w = AGMTILESZ(mv)*mv->esel.w;
				esel_h = AGMTILESZ(mv)*mv->esel.h;
			}
#endif /* EDITION */
		}
	}
next_layer:
	if (++layer < m->nlayers)
		goto draw_layer;			/* Draw next layer */

#ifdef EDITION
	/* Draw the node grid. */
	if (mv->flags & AG_MAPVIEW_GRID) {
		int rx2 = rx;

		for (; ry >= mv->yoffs; ry -= AGMTILESZ(mv)) {
			MAP_ViewHLine(mv, mv->xoffs, rx2, ry);
			for (; rx >= mv->xoffs; rx -= AGMTILESZ(mv))
				MAP_ViewVLine(mv, rx, mv->yoffs, ry);
		}
	}

	/* Indicate the selection. */
	if (esel_x != -1) {
		agPrim.rect_outlined(mv,
		    esel_x, esel_y,
		    esel_w, esel_h,
		    AG_COLOR(MAPVIEW_ESEL_COLOR));
	}
	if (msel_x != -1) {
		agPrim.rect_outlined(mv,
		    msel_x, msel_y,
		    msel_w, msel_h,
		    AG_COLOR(MAPVIEW_MSEL_COLOR));
	}

	/* Draw the cursor for the current tool. */
	if ((mv->flags & AG_MAPVIEW_EDIT) && (mv->mode == AG_MAPVIEW_EDITION) &&
	    (mv->flags & AG_MAPVIEW_NO_CURSOR) == 0 &&
	    (mv->cx != -1 && mv->cy != -1)) {
		draw_cursor(mv);
	}
#endif /* EDITION */

out:
#ifdef HAVE_OPENGL
	if (agView->opengl) {
		if (blend_save) {
			glEnable(GL_BLEND);
		} else {
			glDisable(GL_BLEND);
		}
		glBlendFunc(blend_sfactor, blend_dfactor);
		glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, texenvmode);
	}
#endif
	AG_MutexUnlock(&m->lock);
}

/*
 * Recalculate the offsets to be used by the rendering routine based on
 * the current camera coordinates. 
 */
void
MAP_ViewUpdateCamera(MAP_View *mv)
{
	MAP_Camera *cam;
	int xcam, ycam;

	AG_MutexLock(&mv->map->lock);
	cam = &AGMCAM(mv);
	
	if (cam->x < 0) {
		cam->x = 0;
	} else if (cam->x > mv->map->mapw*AGMTILESZ(mv)) { 
		cam->x = mv->map->mapw*AGMTILESZ(mv);
	}
	if (cam->y < 0) {
		cam->y = 0;
	} else if (cam->y > mv->map->maph*AGMTILESZ(mv)) { 
		cam->y = mv->map->maph*AGMTILESZ(mv);
	}

	xcam = cam->x;
	ycam = cam->y;

	switch (cam->alignment) {
	case AG_MAP_CENTER:
		xcam -= AGWIDGET(mv)->w/2;
		ycam -= AGWIDGET(mv)->h/2;
		break;
	case AG_MAP_LOWER_CENTER:
		xcam -= AGWIDGET(mv)->w/2;
		ycam -= AGWIDGET(mv)->h;
		break;
	case AG_MAP_UPPER_CENTER:
		xcam -= AGWIDGET(mv)->w/2;
		break;
	case AG_MAP_UPPER_LEFT:
		break;
	case AG_MAP_MIDDLE_LEFT:
		ycam -= AGWIDGET(mv)->h/2;
		break;
	case AG_MAP_LOWER_LEFT:
		ycam -= AGWIDGET(mv)->h;
		break;
	case AG_MAP_UPPER_RIGHT:
		xcam -= AGWIDGET(mv)->w;
		break;
	case AG_MAP_MIDDLE_RIGHT:
		xcam -= AGWIDGET(mv)->w;
		ycam -= AGWIDGET(mv)->h/2;
		break;
	case AG_MAP_LOWER_RIGHT:
		xcam -= AGWIDGET(mv)->w;
		ycam -= AGWIDGET(mv)->h;
		break;
	}
	
	mv->mx = xcam / AGMTILESZ(mv);
	if (mv->mx < 0) {
		mv->mx = 0;
		mv->xoffs = -xcam - AGMTILESZ(mv);
	} else {
		mv->xoffs = -(xcam % AGMTILESZ(mv)) - AGMTILESZ(mv);
	}
	
	mv->my = ycam / AGMTILESZ(mv);
	if (mv->my < 0) {
		mv->my = 0;
		mv->yoffs = -ycam - AGMTILESZ(mv);
	} else {
		mv->yoffs = -(ycam % AGMTILESZ(mv)) - AGMTILESZ(mv);
	}
	AG_MutexUnlock(&mv->map->lock);

	if (mv->hbar != NULL) {
		AG_WidgetSetInt(mv->hbar, "min", 0);
		AG_WidgetSetInt(mv->hbar, "max", mv->map->mapw*AGMTILESZ(mv));
		AG_ScrollbarSetBarSize(mv->hbar, 20);
	}
	if (mv->vbar != NULL) {
		AG_WidgetSetInt(mv->vbar, "min", 0);
		AG_WidgetSetInt(mv->vbar, "max", mv->map->maph*AGMTILESZ(mv));
		AG_ScrollbarSetBarSize(mv->vbar, 20);
	}
}

void
MAP_ViewSetScale(MAP_View *mv, Uint zoom, int adj_offs)
{
	int old_tilesz = AGMTILESZ(mv);
	int x, y;
	int old_pixw = mv->map->mapw*AGMTILESZ(mv);
	int old_pixh = mv->map->maph*AGMTILESZ(mv);
	int pixw, pixh;

	if (zoom < ZOOM_MIN) { zoom = ZOOM_MIN; }
	else if (zoom > ZOOM_MAX) { zoom = ZOOM_MAX; }
	
	AGMZOOM(mv) = zoom;
	AGMTILESZ(mv) = zoom*AGTILESZ/100;
	AGMPIXSZ(mv) = AGMTILESZ(mv)/AGTILESZ;

	if (AGMTILESZ(mv) > AG_MAX_TILESZ)
		AGMTILESZ(mv) = AG_MAX_TILESZ;

	mv->mw = AGWIDGET(mv)->w/AGMTILESZ(mv) + 2;
	mv->mh = AGWIDGET(mv)->h/AGMTILESZ(mv) + 2;

	SDL_GetMouseState(&x, &y);
	x -= AGWIDGET(mv)->cx;
	y -= AGWIDGET(mv)->cy;

	pixw = mv->map->mapw*AGMTILESZ(mv);
	pixh = mv->map->maph*AGMTILESZ(mv);

	if (adj_offs) {
		AGMCAM(mv).x = AGMCAM(mv).x * pixw / old_pixw;
		AGMCAM(mv).y = AGMCAM(mv).y * pixh / old_pixh;
	}
	MAP_ViewUpdateCamera(mv);
}

static __inline__ int
inside_nodesel(MAP_View *mv, int x, int y)
{
	return (!agMapviewEditSelOnly || !mv->esel.set ||
	    (x >= mv->esel.x &&
	     y >= mv->esel.y &&
	     x <  mv->esel.x + mv->esel.w &&
	     y <  mv->esel.y + mv->esel.h));
}

static void
toggle_attrib(MAP_View *mv)
{
	MAP_Item *r;
	MAP_Node *node;

	if (mv->attr_x == mv->cx && mv->attr_y == mv->cy)
		return;

	MAP_modBegin(mv->map);
	MAP_modNodeChg(mv->map, mv->cx, mv->cy);

	node = &mv->map->map[mv->cy][mv->cx];
	TAILQ_FOREACH(r, &node->nrefs, nrefs) {
		if (r->layer != mv->map->cur_layer) {
			continue;
		}
		if (r->flags & mv->edit_attr) {
			r->flags &= ~(mv->edit_attr);
		} else {
			r->flags |= mv->edit_attr;
		}
	}

	MAP_modEnd(mv->map);

	mv->attr_x = mv->cx;
	mv->attr_y = mv->cy;
}

static void
mousemotion(AG_Event *event)
{
	MAP_View *mv = AG_SELF();
	int x = AG_INT(1);
	int y = AG_INT(2);
	int xrel = AG_INT(3);
	int yrel = AG_INT(4);
	int state = AG_INT(5);
	int xmap, ymap;
	int rv;

	AG_MutexLock(&mv->map->lock);
	get_node_coords(mv, &x, &y);
	mv->cxrel = x - mv->mouse.x;
	mv->cyrel = y - mv->mouse.y;
	xmap = mv->cx*AGMTILESZ(mv) + mv->cxoffs;
	ymap = mv->cy*AGMTILESZ(mv) + mv->cyoffs;
	mv->mouse.xmap_rel += xmap - mv->mouse.xmap;
	mv->mouse.ymap_rel += ymap - mv->mouse.ymap;
	mv->mouse.xmap = xmap;
	mv->mouse.ymap = ymap;

	if (mv->flags & AG_MAPVIEW_EDIT) {
		if (state & SDL_BUTTON(1) &&
		    mv->cx != -1 && mv->cy != -1 &&
		    (x != mv->mouse.x || y != mv->mouse.y) &&
		    (inside_nodesel(mv, mv->cx, mv->cy))) {
			if (mv->flags & AG_MAPVIEW_SET_ATTRS) {
				toggle_attrib(mv);
				goto out;
			}
			if (mv->mode == AG_MAPVIEW_EDIT_ORIGIN) {
				mv->map->origin.x = mv->cx;
				mv->map->origin.y = mv->cy;
				mv->map->origin.layer = mv->map->cur_layer;
				goto out;
			}
			if (mv->curtool != NULL &&
			    mv->curtool->ops->effect != NULL &&
			    (rv = mv->curtool->ops->effect(mv->curtool,
			     &mv->map->map[mv->cy][mv->cx])) != -1) {
				mv->map->nmods += rv;
				goto out;
			}
		}
		if (mv->curtool != NULL &&
		    mv->curtool->ops->mousemotion != NULL &&
		    mv->curtool->ops->mousemotion(mv->curtool,
		      mv->mouse.xmap, mv->mouse.ymap,
		      xrel, yrel, state) == 1) {
			goto out;
		}
		if (mv->deftool != NULL &&
		    mv->deftool->ops->mousemotion != NULL &&
		    mv->deftool->ops->mousemotion(mv->deftool,
		      mv->mouse.xmap, mv->mouse.ymap,
		      xrel, yrel, state) == 1) {
			goto out;
		}
	}
	
	if (mv->mouse.scrolling) {
		AGMCAM(mv).x -= xrel;
		AGMCAM(mv).y -= yrel;
		MAP_ViewUpdateCamera(mv);
	} else if (mv->msel.set) {
		mv->msel.xoffs += mv->cxrel;
		mv->msel.yoffs += mv->cyrel;
	} else if (mv->esel.set && mv->esel.moving) {
		MAP_NodeselUpdateMove(mv, mv->cxrel, mv->cyrel);
	} else if (mv->rsel.moving) {
		if (abs(mv->mouse.xmap_rel) > AGMPIXSZ(mv)) {
			refsel_update(mv, mv->mouse.xmap_rel < 0 ? -1 : 1, 0);
			mv->mouse.xmap_rel = 0;
		}
		if (abs(mv->mouse.ymap_rel) > AGMPIXSZ(mv)) {
			refsel_update(mv, 0, mv->mouse.ymap_rel < 0 ? -1 : 1);
			mv->mouse.ymap_rel = 0;
		}
	}
out:
	mv->mouse.x = x;
	mv->mouse.y = y;
	AG_MutexUnlock(&mv->map->lock);
}

void
MAP_ViewSetMode(MAP_View *mv, enum map_view_mode mode)
{
	mv->mode = mode;
}

static void
mousebuttondown(AG_Event *event)
{
	MAP_View *mv = AG_SELF();
	MAP *m = mv->map;
	int button = AG_INT(1);
	int x = AG_INT(2);
	int y = AG_INT(3);
	MAP_Tool *tool;
	int rv;
	
	AG_WidgetFocus(mv);
	
	AG_MutexLock(&m->lock);
	get_node_coords(mv, &x, &y);
	mv->mouse.x = x;
	mv->mouse.y = y;
	mv->mouse.xmap = mv->cx*AGMTILESZ(mv) + mv->cxoffs;
	mv->mouse.ymap = mv->cy*AGMTILESZ(mv) + mv->cyoffs;
	mv->mouse.xmap_rel = 0;
	mv->mouse.ymap_rel = 0;
	
	if (mv->actor != NULL &&
	    AGACTOR_OPS(mv->actor)->mousebuttondown != NULL &&
	    AGACTOR_OPS(mv->actor)->mousebuttondown(mv->actor,
	      mv->mouse.xmap, mv->mouse.ymap, button) == -1)
		goto out;

	if ((mv->flags & AG_MAPVIEW_EDIT) &&
	    (mv->cx >= 0 && mv->cy >= 0)) {
		if (mv->mode == AG_MAPVIEW_EDIT_ATTRS &&
		    button == SDL_BUTTON_LEFT) {
			mv->flags |= AG_MAPVIEW_SET_ATTRS;
			mv->attr_x = -1;
			mv->attr_y = -1;
			toggle_attrib(mv);
			goto out;
		}
		if (mv->flags & AG_MAPVIEW_SHOW_ORIGIN &&
		    button == SDL_BUTTON_LEFT &&
		    mv->cx == m->origin.x &&
		    mv->cy == m->origin.y) {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDIT_ORIGIN);
			goto out;
		}
		if (mv->curtool != NULL) {
			if (mv->curtool->ops->mousebuttondown != NULL &&
			    mv->curtool->ops->mousebuttondown(mv->curtool,
			      mv->mouse.xmap, mv->mouse.ymap, button) == 1) {
				goto out;
			}
			if (button == SDL_BUTTON_LEFT &&
			    mv->curtool->ops->effect != NULL &&
			    inside_nodesel(mv, mv->cx, mv->cy)) {
				if ((rv = mv->curtool->ops->effect(mv->curtool,
				     &m->map[mv->cy][mv->cx])) != -1) {
					mv->map->nmods = rv;
					goto out;
				}
			}
		}

		/* Mouse bindings allow inactive tools to bind mouse events. */
		TAILQ_FOREACH(tool, &mv->tools, tools) {
			MAP_ToolMouseBinding *mbinding;

			SLIST_FOREACH(mbinding, &tool->mbindings, mbindings) {
				if (mbinding->button != button) {
					continue;
				}
				if (mbinding->edit &&
				   (AGOBJECT(m)->flags & AG_OBJECT_READONLY)) {
					continue;
				}
				tool->mv = mv;
				if (mbinding->func(tool, button, 1,
				    mv->mouse.xmap, mv->mouse.ymap,
				    mbinding->arg) == 1)
					goto out;
			}
		}

		if (mv->deftool != NULL &&
		    mv->deftool->ops->mousebuttondown != NULL &&
		    mv->deftool->ops->mousebuttondown(mv->deftool,
		      mv->mouse.xmap, mv->mouse.ymap, button) == 1) {
			goto out;
		}
	}

	switch (button) {
	case SDL_BUTTON_LEFT:
		if (mv->esel.set) {
			if (mv->cx >= mv->esel.x &&
			    mv->cy >= mv->esel.y &&
			    mv->cx < mv->esel.x+mv->esel.w &&
			    mv->cy < mv->esel.y+mv->esel.h) {
				MAP_NodeselBeginMove(mv);
			} else {
				mv->esel.set = 0;
			}
			goto out;
		} else {
			SDLMod mod = SDL_GetModState();
			MAP_Item *r;
			int nx, ny;
			
			if (mv->curtool != NULL &&
			    mv->curtool->ops == &agMapNodeselOps &&
			    (mv->flags & AG_MAPVIEW_NO_NODESEL) == 0) {
				MAP_NodeselBegin(mv);
				goto out;
			}
			if ((mod & KMOD_CTRL) == 0) {
				/* XXX too expensive */
				for (ny = 0; ny < m->maph; ny++) {
					for (nx = 0; nx < m->mapw; nx++) {
						MAP_Node *node =
						    &m->map[ny][nx];
						
						TAILQ_FOREACH(r, &node->nrefs,
						    nrefs) {
							r->flags &=
							   ~(AG_NITEM_SELECTED);
						}
					}
				}
			}
			if (mv->curtool != NULL &&
			    mv->curtool->ops == &agMapRefselOps &&
			    (r = MAP_ItemLocate(m, mv->mouse.xmap,
			    mv->mouse.ymap, mv->cam)) != NULL) {
				if (r->flags & AG_NITEM_SELECTED) {
					r->flags &= ~(AG_NITEM_SELECTED);
				} else {
					r->flags |= AG_NITEM_SELECTED;
					mv->rsel.moving = 1;
				}
			}
		}
		if (mv->dblclicked) {
			AG_CancelEvent(mv, "dblclick-expire");
			AG_PostEvent(NULL, mv, "mapview-dblclick",
			    "%i, %i, %i, %i, %i", button, x, y,
			    mv->cxoffs, mv->cyoffs);
			mv->dblclicked = 0;
		} else {
			mv->dblclicked++;
			AG_SchedEvent(NULL, mv, agMouseDblclickDelay,
			    "dblclick-expire", NULL);
		}
		break;
	case SDL_BUTTON_MIDDLE:
		/* TODO menu */
		if ((mv->flags & AG_MAPVIEW_EDIT) == 0 ||
		    mv->curtool == NULL) {
		    	mv->mouse.scrolling++;
			break;
		}
		break;
	case SDL_BUTTON_RIGHT:
		mv->mouse.scrolling++;
		goto out;
	case SDL_BUTTON_WHEELDOWN:
		if ((mv->flags & AG_MAPVIEW_NO_BMPSCALE) == 0) {
			MAP_ViewSetScale(mv, AGMZOOM(mv) - agMapviewZoomInc,
			    1);
			MAP_ViewStatus(mv, _("%d%% zoom"), AGMZOOM(mv));
		}
		break;
	case SDL_BUTTON_WHEELUP:
		if ((mv->flags & AG_MAPVIEW_NO_BMPSCALE) == 0) {
			MAP_ViewSetScale(mv, AGMZOOM(mv) + agMapviewZoomInc,
			    1);
			MAP_ViewStatus(mv, _("%d%% zoom"), AGMZOOM(mv));
		}
		break;
	}
out:
	AG_MutexUnlock(&m->lock);
}

static void
mousebuttonup(AG_Event *event)
{
	MAP_View *mv = AG_SELF();
	MAP *m = mv->map;
	int button = AG_INT(1);
	int x = AG_INT(2);
	int y = AG_INT(3);
	MAP_Tool *tool;
	
	AG_MutexLock(&m->lock);
	get_node_coords(mv, &x, &y);

	mv->flags &= ~(AG_MAPVIEW_SET_ATTRS);
	
	if (mv->actor != NULL &&
	    AGACTOR_OPS(mv->actor)->mousebuttonup != NULL) {
		x = mv->cx*AGMTILESZ(mv) + mv->cxoffs;
		y = mv->cy*AGMTILESZ(mv) + mv->cyoffs;
		if (AGACTOR_OPS(mv->actor)->mousebuttonup(mv->actor, x, y,
		    button) == -1)
			goto out;
	}

	if ((mv->flags & AG_MAPVIEW_EDIT) &&
	    (mv->cx >= 0 && mv->cy >= 0)) {
	    	if (mv->mode == AG_MAPVIEW_EDIT_ORIGIN) {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDITION);
		}
		if (mv->curtool != NULL) {
			if (mv->curtool->ops->mousebuttonup != NULL &&
			    mv->curtool->ops->mousebuttonup(mv->curtool,
			      mv->mouse.xmap, mv->mouse.ymap, button) == 1) {
				goto out;
			}
		}
		
		TAILQ_FOREACH(tool, &mv->tools, tools) {
			MAP_ToolMouseBinding *mbinding;

			SLIST_FOREACH(mbinding, &tool->mbindings, mbindings) {
				if (mbinding->button != button) {
					continue;
				}
				if (mbinding->edit &&
				    (AGOBJECT(m)->flags&AG_OBJECT_READONLY)) {
					continue;
				}
				tool->mv = mv;
				if (mbinding->func(tool, button, 0, x, y,
				    mbinding->arg) == 1)
					goto out;
			}
		}
		
		if (mv->deftool != NULL &&
		    mv->deftool->ops->mousebuttonup != NULL &&
		    mv->deftool->ops->mousebuttonup(mv->deftool,
		      mv->mouse.xmap, mv->mouse.ymap, button) == 1) {
			goto out;
		}
	} else {
		mv->mouse.scrolling = 0;
		if (mv->esel.set && mv->esel.moving) {
			MAP_NodeselEndMove(mv);
		}
		mv->rsel.moving = 0;
		goto out;
	}

	switch (button) {
	case SDL_BUTTON_LEFT:
		if (mv->msel.set &&
		   (mv->msel.xoffs == 0 || mv->msel.yoffs == 0)) {
			mv->esel.set = 0;
			mv->msel.set = 0;
		} else {
			if (mv->msel.set) {
				MAP_NodeselEnd(mv);
				mv->msel.set = 0;
			} else if (mv->esel.set && mv->esel.moving) {
				MAP_NodeselEndMove(mv);
			}
		}
		mv->rsel.moving = 0;
		break;
	case SDL_BUTTON_RIGHT:
	case SDL_BUTTON_MIDDLE:
		mv->mouse.scrolling = 0;
		break;
	}
out:
	AG_MutexUnlock(&m->lock);
}

static void
key_up(AG_Event *event)
{
	MAP_View *mv = AG_SELF();
	int keysym = AG_INT(1);
	int keymod = AG_INT(2);
	MAP_Tool *tool;
	
	AG_MutexLock(&mv->map->lock);

	if (mv->actor != NULL &&
	    AGACTOR_OPS(mv->actor)->keyup != NULL) {
		if (AGACTOR_OPS(mv->actor)->keyup(mv->actor, keysym, keymod)
		    == -1)
			goto out;
	}
	
	if (mv->flags & AG_MAPVIEW_EDIT &&
	    mv->curtool != NULL &&
	    mv->curtool->ops->keyup != NULL &&
	    mv->curtool->ops->keyup(mv->curtool, keysym, keymod) == 1)
		goto out;
	
	TAILQ_FOREACH(tool, &mv->tools, tools) {
		MAP_ToolKeyBinding *kbinding;

		SLIST_FOREACH(kbinding, &tool->kbindings, kbindings) {
			if (kbinding->key == keysym &&
			    (kbinding->mod == KMOD_NONE ||
			     keymod & kbinding->mod)) {
				if (kbinding->edit &&
				   (((mv->flags & AG_MAPVIEW_EDIT) == 0) ||
				    ((AGOBJECT(mv->map)->flags &
				      AG_OBJECT_READONLY)))) {
					continue;
				}
				tool->mv = mv;
				if (kbinding->func(tool, keysym, 0,
				    kbinding->arg) == 1)
					goto out;
			}
		}
	}
out:
	AG_MutexUnlock(&mv->map->lock);
}

static void
key_down(AG_Event *event)
{
	MAP_View *mv = AG_SELF();
	int keysym = AG_INT(1);
	int keymod = AG_INT(2);
	MAP_Tool *tool;
	
	AG_MutexLock(&mv->map->lock);

	if (mv->actor != NULL &&
	    AGACTOR_OPS(mv->actor)->keydown != NULL) {
		if (AGACTOR_OPS(mv->actor)->keydown(mv->actor, keysym, keymod)
		    == -1)
			goto out;
	}
	
	if (mv->flags & AG_MAPVIEW_EDIT &&
	    mv->curtool != NULL &&
	    mv->curtool->ops->keydown != NULL &&
	    mv->curtool->ops->keydown(mv->curtool, keysym, keymod) == 1)
		goto out;

	TAILQ_FOREACH(tool, &mv->tools, tools) {
		MAP_ToolKeyBinding *kbinding;

		SLIST_FOREACH(kbinding, &tool->kbindings, kbindings) {
			if (kbinding->key == keysym &&
			    (kbinding->mod == KMOD_NONE ||
			     keymod & kbinding->mod)) {
				if (kbinding->edit &&
				   (((mv->flags & AG_MAPVIEW_EDIT) == 0) ||
				    ((AGOBJECT(mv->map)->flags &
				      AG_OBJECT_READONLY)))) {
					continue;
				}
				tool->mv = mv;
				if (kbinding->func(tool, keysym, 1,
				    kbinding->arg) == 1)
					goto out;
			}
		}
	}

	switch (keysym) {
	case SDLK_z:
		if (keymod & KMOD_CTRL) {
			MAP_Undo(mv->map);
		}
		break;
	case SDLK_r:
		if (keymod & KMOD_CTRL) {
			MAP_Redo(mv->map);
		}
		break;
	case SDLK_1:
	case SDLK_0:
		if ((mv->flags & AG_MAPVIEW_NO_BMPSCALE) == 0) {
			MAP_ViewSetScale(mv, 100, 1);
			MAP_ViewStatus(mv, _("%d%% zoom"), AGMZOOM(mv));
		}
		break;
	case SDLK_o:
		center_to_origin(mv);
		break;
	case SDLK_g:
		if (mv->flags & AG_MAPVIEW_GRID) {
			mv->flags &= ~(AG_MAPVIEW_GRID);
		} else {
			mv->flags |= AG_MAPVIEW_GRID;
		}
		break;
	case SDLK_b:
		if (mv->flags & AG_MAPVIEW_NO_BG) {
			mv->flags &= ~(AG_MAPVIEW_NO_BG);
		} else {
			mv->flags |= AG_MAPVIEW_NO_BG;
		}
		break;
	case SDLK_w:
		if (mv->mode == AG_MAPVIEW_EDITION) {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDIT_ATTRS);
			mv->edit_attr = AG_NITEM_BLOCK;
		} else {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDITION);
		}
		break;
	case SDLK_c:
		if (mv->mode == AG_MAPVIEW_EDITION) {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDIT_ATTRS);
			mv->edit_attr = AG_NITEM_CLIMBABLE;
		} else {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDITION);
		}
		break;
	case SDLK_s:
		if (mv->mode == AG_MAPVIEW_EDITION) {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDIT_ATTRS);
			mv->edit_attr = AG_NITEM_SLIPPERY;
		} else {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDITION);
		}
		break;
	case SDLK_j:
		if (mv->mode == AG_MAPVIEW_EDITION) {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDIT_ATTRS);
			mv->edit_attr = AG_NITEM_JUMPABLE;
		} else {
			MAP_ViewSetMode(mv, AG_MAPVIEW_EDITION);
		}
		break;
	}
out:	
	AG_MutexUnlock(&mv->map->lock);
}

void
MAP_ViewScale(void *p, int rw, int rh)
{
	MAP_View *mv = p;

	if (rw == -1 && rh == -1) {
		AGWIDGET(mv)->w = mv->prew*AGTILESZ;
		AGWIDGET(mv)->h = mv->preh*AGTILESZ;
		if (mv->hbar != NULL) {
			AGWIDGET_OPS(mv->hbar)->scale(mv->hbar, -1, -1);
		}
		if (mv->vbar != NULL) {
			AGWIDGET_OPS(mv->vbar)->scale(mv->vbar, -1, -1);
		}
		return;
	}

	if (mv->hbar != NULL) {
		AGWIDGET(mv->hbar)->x = 0;
		AGWIDGET(mv->hbar)->y = AGWIDGET(mv)->h - mv->hbar->bw;
		AGWIDGET(mv->hbar)->w = AGWIDGET(mv)->w;
		AGWIDGET(mv->hbar)->h = mv->hbar->bw;
		AG_WidgetScale(mv->hbar, AGWIDGET(mv->hbar)->w,
		   AGWIDGET(mv->hbar)->h);
	}
	if (mv->vbar != NULL) {
		AGWIDGET(mv->vbar)->x = 0;
		AGWIDGET(mv->vbar)->y = 0;
		AGWIDGET(mv->vbar)->w = mv->vbar->bw;
		AGWIDGET(mv->vbar)->h = AGWIDGET(mv)->h;
		AG_WidgetScale(mv->vbar, AGWIDGET(mv->vbar)->w,
		    AGWIDGET(mv->vbar)->h);
	}
	
	AG_MutexLock(&mv->map->lock);
	MAP_ViewSetScale(mv, AGMZOOM(mv), 0);
	AG_MutexUnlock(&mv->map->lock);
}

static void
lost_focus(AG_Event *event)
{
	MAP_View *mv = AG_SELF();

	AG_CancelEvent(mv, "dblclick-expire");
	if (mv->actor != NULL)
		AG_ObjectCancelTimeouts(mv->actor, 0);
}

/* Set the coordinates and geometry of the selection rectangle. */
void
MAP_ViewSetSelection(MAP_View *mv, int x, int y, int w, int h)
{
	mv->msel.set = 0;
	mv->esel.set = 1;
	mv->esel.x = x;
	mv->esel.y = y;
	mv->esel.w = w;
	mv->esel.h = h;
}

/* Fetch the coordinates and geometry of the selection rectangle. */
int
MAP_ViewGetSelection(MAP_View *mv, int *x, int *y, int *w, int *h)
{
	if (mv->esel.set) {
		if (x != NULL) { *x = mv->esel.x; }
		if (y != NULL) { *y = mv->esel.y; }
		if (w != NULL) { *w = mv->esel.w; }
		if (h != NULL) { *h = mv->esel.h; }
		return (1);
	} else {
		return (0);
	}
}

void	
MAP_ViewPrescale(MAP_View *mv, int w, int h)
{
	mv->prew = w;
	mv->preh = h;
}

void
MAP_ViewStatus(MAP_View *mv, const char *fmt, ...)
{
	char status[AG_LABEL_MAX];
	va_list ap;

	if (mv->status == NULL)
		return;

	va_start(ap, fmt);
	vsnprintf(status, sizeof(status), fmt, ap);
	va_end(ap);

	AG_WidgetReplaceSurface(mv->status, mv->status->surface,
	    AG_TextRender(NULL, -1, AG_COLOR(TEXT_COLOR), status));
}