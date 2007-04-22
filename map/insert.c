/*	$Csoft: insert.c,v 1.13 2005/08/29 05:27:28 vedge Exp $	*/

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
#include <agar/rg/tileset.h>

#include <agar/gui/radio.h>
#include <agar/gui/checkbox.h>
#include <agar/gui/spinbutton.h>
#include <agar/gui/label.h>
#include <agar/gui/tlist.h>
#include <agar/gui/primitive.h>
#include <agar/gui/notebook.h>
#include <agar/gui/combo.h>

#include "map.h"
#include "mapedit.h"
#include "insert.h"
#include "tools.h"

#include <string.h>

static void
insert_init(void *p)
{
	struct map_insert_tool *ins = p;

	ins->snap_mode = AG_GFX_SNAP_NOT;
	ins->replace_mode = 0;
	ins->angle = 0;
	MAP_Init(&ins->mTmp, "tmp");
	ins->mvTmp = NULL;

	MAP_ToolPushStatus(ins,
	    _("Select position on map ($(L)=Insert, $(M)=Rotate)"));
}

static void
insert_destroy(void *p)
{
	struct map_insert_tool *ins = p;

	MAP_Destroy(&ins->mTmp);
}

static void
snap_sprite(struct map_insert_tool *ins, MAP_Item *r, AG_Sprite *spr)
{
	MAP_View *mv = TOOL(ins)->mv;

	switch (ins->snap_mode) {
	case AG_GFX_SNAP_NOT:
		r->r_gfx.xcenter += mv->cxoffs*AGTILESZ/AGMTILESZ(mv);
		r->r_gfx.ycenter += mv->cyoffs*AGTILESZ/AGMTILESZ(mv);
		break;
	default:
		break;
	}

}

static void
generate_map(struct map_insert_tool *ins, AG_Sprite *spr)
{
	int sy, sx, dx, dy;
	int sw = spr->su->w/AGTILESZ;
	int sh = spr->su->h/AGTILESZ;
	int nw, nh;

	if (spr->su->w%AGTILESZ > 0) sw++;
	if (spr->su->h%AGTILESZ > 0) sh++;

	MAP_AllocNodes(&ins->mTmp, sw, sh);
	ins->mTmp.origin.x = spr->xOrig/AGTILESZ;
	ins->mTmp.origin.y = spr->yOrig/AGTILESZ;
	for (sy = 0, dy = 0;
	     sy < spr->su->h;
	     sy += AGTILESZ, dy++) {
		for (sx = 0, dx = 0;
		     sx < spr->su->w;
		     sx += AGTILESZ, dx++) {
			MAP_Node *dn;
			MAP_Item *r;
			int dw, dh, nlayer;

			if (dx >= ins->mTmp.mapw ||
			    dy >= ins->mTmp.maph)
				continue;

			dn = &ins->mTmp.map[dy][dx];
			dw = spr->su->w - sx;
			dh = spr->su->h - sy;

			r = Malloc(sizeof(MAP_Item), M_MAP_NITEM);
			MAP_ItemInit(r, AG_NITEM_SPRITE);
			MAP_ItemSetSprite(r, &ins->mTmp, spr->pgfx->pobj,
			    spr->index);

			r->r_gfx.rs.x = dx*AGTILESZ;
			r->r_gfx.rs.y = dy*AGTILESZ;
			r->r_gfx.rs.w = (dw >= AGTILESZ) ? AGTILESZ : dw;
			r->r_gfx.rs.h = (dh >= AGTILESZ) ? AGTILESZ : dh;
			r->flags |= AG_SPRITE_ATTR2(spr,dx,dy);

			nlayer = AG_SPRITE_LAYER2(spr,dx,dy);
			if (nlayer < 0) {
				nlayer = 0;
			} else {
				if (nlayer >= ins->mTmp.nlayers)
					MAP_PushLayer(&ins->mTmp, "");
			}
			r->layer = nlayer;
			
			/* XXX also need to rotate the whole map */
	//		AG_TransformRotate(r, ins->angle);

			TAILQ_INSERT_TAIL(&dn->nrefs, r, nrefs);
		}
	}
}

static void
insert_pane(void *p, void *con)
{
	struct map_insert_tool *ins = p;
	AG_Radio *rad;
	AG_Checkbox *cb;
	AG_Spinbutton *sb;
	AG_Combo *com;
	AG_TlistItem *it;
	MAP_View *mvMain = TOOL(ins)->mv;
	MAP_View *mv;
	AG_Sprite *spr;
	AG_Notebook *nb;
	AG_NotebookTab *ntab;

	if ((it = AG_TlistSelectedItem(mvMain->lib_tl)) == NULL ||
	     strcmp(it->cat, "tile") != 0) {
		return;
	}
	spr = it->p1;
	
	nb = AG_NotebookNew(con, AG_NOTEBOOK_VFILL|AG_NOTEBOOK_HFILL);
	ntab = AG_NotebookAddTab(nb, _("Tiles"), AG_BOX_VERT);
	mv = ins->mvTmp = MAP_ViewNew(ntab, &ins->mTmp,
	    AG_MAPVIEW_EDIT|AG_MAPVIEW_GRID, NULL, NULL);
	MAP_ViewPrescale(mv, 7, 7);
	MAP_ViewSelectTool(mv,
	    MAP_ViewRegTool(mv, &agMapNodeselOps, &ins->mTmp),
	    &ins->mTmp);
	generate_map(ins, spr);
	
	ntab = AG_NotebookAddTab(nb, _("Settings"), AG_BOX_VERT);
	{
		AG_LabelNew(ntab, AG_LABEL_STATIC, _("Snap to: "));
		rad = AG_RadioNew(ntab, AG_RADIO_HFILL, agGfxSnapNames);
		AG_WidgetBind(rad, "value", AG_WIDGET_INT, &ins->snap_mode);

		cb = AG_CheckboxNew(ntab, 0, _("Replace mode"));
		AG_WidgetBind(cb, "state", AG_WIDGET_INT, &ins->replace_mode);

		sb = AG_SpinbuttonNew(ntab, 0, _("Rotation: "));
		AG_WidgetBind(sb, "value", AG_WIDGET_INT, &ins->angle);
		AG_SpinbuttonSetRange(sb, 0, 360);
		AG_SpinbuttonSetIncrement(sb, 90);
	}
}

static int
insert_effect(void *p, MAP_Node *node)
{
	struct map_insert_tool *ins = p;
	MAP_View *mv = TOOL(ins)->mv;
	MAP *mSrc = &ins->mTmp;
	MAP *mDst = mv->map;
	int sx, sy, sx0, sy0, sx1, sy1;
	int dx, dy, dx0, dy0;
	int l;
	AG_TlistItem *it;
	AG_Sprite *spr;
	
	if (mv->lib_tl == NULL ||
	    (it = AG_TlistSelectedItem(mv->lib_tl)) == 0 ||
	    strcmp(it->cat, "tile") != 0) {
		return (1);
	}
	spr = it->p1;

	if (ins->mvTmp->esel.set) {
		sx0 = ins->mvTmp->esel.x;
		sy0 = ins->mvTmp->esel.y;
		sx1 = sx0 + ins->mvTmp->esel.w - 1;
		sy1 = sy0 + ins->mvTmp->esel.h - 1;
		dx0 = mv->cx;
		dy0 = mv->cy;
	} else {
		sx0 = 0;
		sy0 = 0;
		sx1 = mSrc->mapw-1;
		sy1 = mSrc->maph-1;
		dx0 = mv->cx - mSrc->origin.x;
		dy0 = mv->cy - mSrc->origin.y;
	}

	MAP_modBegin(mDst);
	for (sy = sy0, dy = dy0;
	     sy <= sy1 && dy < mDst->maph;
	     sy++, dy++) {
		for (sx = sx0, dx = dx0;
		     sx <= sx1 && dx < mDst->mapw;
		     sx++, dx++) {
			MAP_Node *sn, *dn;
			MAP_Item *r1, *r2;

			if (dx < 0 || dx >= mDst->mapw ||
			    dy < 0 || dy >= mDst->maph) {
				continue;
			}
			sn = &mSrc->map[sy][sx]; 
			dn = &mDst->map[dy][dx];
			
			MAP_modNodeChg(mDst, dx, dy);

			if (ins->replace_mode) {
				MAP_NodeRemoveAll(mDst, dn, mDst->cur_layer);
			}
			TAILQ_FOREACH(r1, &sn->nrefs, nrefs) {
				r2 = MAP_NodeCopyItem(r1, mDst, dn, -1);
				r2->layer += mDst->cur_layer;
				while (r2->layer >= mDst->nlayers) {
					if (MAP_PushLayer(mDst, "") == 0)
						MAP_modLayerAdd(mDst,
						    mDst->nlayers - 1);
				}
				if (ins->snap_mode == AG_GFX_SNAP_NOT) {
					r2->r_gfx.xcenter +=
					    mv->cxoffs*AGTILESZ/AGMTILESZ(mv);
					r2->r_gfx.ycenter +=
					    mv->cyoffs*AGTILESZ/AGMTILESZ(mv);
				}
			}
		}
	}
	MAP_modEnd(mDst);
	return (1);
}

static int
insert_cursor(void *p, SDL_Rect *rd)
{
	struct map_insert_tool *ins = p;
	MAP_View *mv = TOOL(ins)->mv;
	MAP *mSrc = &ins->mTmp;
	AG_TlistItem *it;
	AG_Sprite *spr;
	int sx0, sy0, sx1, sy1;
	int dx0, dy0, dx1, dy1;
	int dx, dy, sx, sy;

	if (mv->lib_tl == NULL ||
	   (it = AG_TlistSelectedItem(mv->lib_tl)) == NULL ||
	   strcmp(it->cat, "tile") != 0 ||
	   (spr = it->p1) == NULL || spr->su == NULL) {
		return (-1);
	}

	if (ins->snap_mode == AG_GFX_SNAP_NOT) {
		agPrim.rect_outlined(mv, rd->x+1, rd->y+1,
		    AGMTILESZ(mv)-1, AGMTILESZ(mv)-1,
		    AG_COLOR(MAPVIEW_GRID_COLOR));
	}
	
	if (ins->mvTmp->esel.set) {
		sx0 = ins->mvTmp->esel.x;
		sy0 = ins->mvTmp->esel.y;
		sx1 = sx0 + ins->mvTmp->esel.w - 1;
		sy1 = sy0 + ins->mvTmp->esel.h - 1;
		dx0 = AGWIDGET(mv)->cx + rd->x;
		dy0 = AGWIDGET(mv)->cy + rd->y;
	} else {
		sx0 = 0;
		sy0 = 0;
		sx1 = mSrc->mapw-1;
		sy1 = mSrc->maph-1;
		dx0 = AGWIDGET(mv)->cx + rd->x - mSrc->origin.x*AGMTILESZ(mv);
		dy0 = AGWIDGET(mv)->cy + rd->y - mSrc->origin.y*AGMTILESZ(mv);
	}
	if (ins->snap_mode == AG_GFX_SNAP_NOT) {
		dx0 += mv->cxoffs*AGTILESZ/AGMTILESZ(mv);
		dy0 += mv->cyoffs*AGTILESZ/AGMTILESZ(mv);
	}
	for (sy = sy0, dy = dy0; sy <= sy1; sy++, dy += AGMTILESZ(mv)) {
		for (sx = sx0, dx = dx0; sx <= sx1; sx++, dx += AGMTILESZ(mv)) {
			MAP_Node *sn = &mSrc->map[sy][sx];
			MAP_Item *r;

			TAILQ_FOREACH(r, &sn->nrefs, nrefs)
				MAP_ItemDraw(mv->map, r, dx, dy, mv->cam);
		}
	}
	return (0);
}

static int
insert_mousebuttondown(void *p, int x, int y, int btn)
{
	struct map_insert_tool *ins = p;

	if (btn == SDL_BUTTON_MIDDLE) {
		ins->angle = (ins->angle + 90) % 360;
		return (1);
	}
	return (0);
}

static int
insert_mousemotion(void *p, int x, int y, int xrel, int yrel, int btn)
{
	struct map_insert_tool *ins = p;
	MAP_View *mv = TOOL(ins)->mv;
	int nx = x/AGMTILESZ(mv);
	int ny = y/AGMTILESZ(mv);
	AG_TlistItem *it;
	AG_Object *ob;

	if (nx == mv->mouse.x && ny == mv->mouse.y)
		return (0);

	if ((it = AG_TlistSelectedItem(mv->lib_tl)) == NULL ||
	    mv->cx == -1 || mv->cy == -1) {
		return (1);
	}
	if (strcmp(it->cat, "tile") == 0) {
		MAP_ToolSetStatus(ins,
		    _("Insert %s tile at [%d,%d] ($(L)=Confirm, $(M)=Rotate)."),
		    it->text, mv->cx, mv->cy);
	}
	return (0);
}

const MAP_ToolOps agMapInsertOps = {
	"Insert", N_("Insert node element"),
	STAMP_TOOL_ICON,
	sizeof(struct map_insert_tool),
	TOOL_HIDDEN,
	insert_init,
	insert_destroy,
	insert_pane,
	NULL,				/* edit */
	insert_cursor,
	insert_effect,

	insert_mousemotion,
	insert_mousebuttondown,
	NULL,				/* mousebuttonup */
	NULL,				/* keydown */
	NULL				/* keyup */
};