/*	$Csoft: fill.c,v 1.14 2005/08/27 04:34:05 vedge Exp $	*/

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

#include <agar/compat/arc4random.h>
#include <agar/core/core.h>
#include <agar/rg/tileset.h>

#include <agar/gui/radio.h>
#include <agar/gui/checkbox.h>
#include <agar/gui/tlist.h>

#include "map.h"
#include "mapedit.h"

#include <string.h>

struct rg_fill_tool {
	MAP_Tool tool;
	enum fill_mode {
		FILL_FROM_CLIPBRD,
		FILL_FROM_ART,
		FILL_CLEAR
	} mode;
	int randomize_angle;
};

static void
init(void *p)
{
	struct rg_fill_tool *fi = p;

	fi->mode = FILL_FROM_ART;
	fi->randomize_angle = 0;
	
	MAP_ToolPushStatus(fi, _("Select a node and fill with $(L)."));
}

static void
pane(void *p, void *con)
{
	struct rg_fill_tool *fi = p;
	static const char *mode_items[] = {
		N_("Clipboard pattern"),
		N_("Source artwork"),
		N_("Clear"),
		NULL
	};
	AG_Radio *rad;
	AG_Checkbox *cb;

	rad = AG_RadioNew(con, AG_RADIO_HFILL, mode_items);
	AG_WidgetBind(rad, "value", AG_WIDGET_INT, &fi->mode);

	cb = AG_CheckboxNew(con, 0, _("Randomize angle"));
	AG_WidgetBind(cb, "state", AG_WIDGET_BOOL, &fi->randomize_angle);
}

static int
effect(void *p, MAP_Node *n)
{
	struct rg_fill_tool *fi = p;
	MAP_View *mv = TOOL(fi)->mv;
	MAP *m = mv->map;
	MAP *copybuf = &agMapEditor.copybuf;
	int sx = 0, sy = 0, dx = 0, dy = 0;
	int dw = m->mapw, dh = m->maph;
	int x, y, angle = 0, i = 0;
	AG_TlistItem *it;
	AG_Sprite *spr;
	Uint32 rand = 0;
	Uint8 byte = 0;

	MAP_ViewGetSelection(mv, &dx, &dy, &dw, &dh);
	MAP_modBegin(m);

	switch (fi->mode) {
	case FILL_FROM_CLIPBRD:
		if (copybuf->mapw == 0 || copybuf->maph == 0) {
			AG_TextMsg(AG_MSG_ERROR, _("The clipboard is empty."));
			return (1);
		}
		for (y = dy; y < dy+dh; y++) {
			for (x = dx; x < dx+dw; x++) {
				MAP_Node *sn = &copybuf->map[sy][sx];
				MAP_Node *dn = &m->map[y][x];
				MAP_Item *r;

				MAP_modNodeChg(m, x, y);
				MAP_NodeRemoveAll(m, dn, m->cur_layer);
				TAILQ_FOREACH(r, &sn->nrefs, nrefs) {
					MAP_NodeCopyItem(r, m, dn, m->cur_layer);
				}
				if (++sx >= copybuf->mapw)
					sx = 0;
			}
		}
		if (++sy >= copybuf->maph) {
			sy = 0;
		}
		break;
	case FILL_FROM_ART:
		if (mv->lib_tl == NULL ||
		    (it = AG_TlistSelectedItem(mv->lib_tl)) == NULL ||
		    strcmp(it->cat, "tile") != 0) {
			break;
		}
		spr = it->p1;
		
		for (y = dy; y < dy+dh; y++) {
			for (x = dx; x < dx+dw; x++) {
				MAP_Node *n = &m->map[y][x];
				MAP_Item *r;

				MAP_modNodeChg(m, x, y);
				MAP_NodeRemoveAll(m, n, m->cur_layer);
				r = Malloc(sizeof(MAP_Item),
				    M_MAP_NITEM);
				MAP_ItemInit(r, AG_NITEM_SPRITE);
				MAP_ItemSetSprite(r, m, spr->pgfx->pobj,
				    spr->index);
				MAP_ItemSetLayer(r, m->cur_layer);
				r->r_gfx.xorigin = spr->xOrig;
				r->r_gfx.yorigin = spr->yOrig;
				r->r_gfx.xcenter = AGTILESZ/2;
				r->r_gfx.ycenter = AGTILESZ/2;
				TAILQ_INSERT_TAIL(&n->nrefs, r, nrefs);
	
#if 0
				if (fi->randomize_angle) {
					switch (i++) {
					case 0:
						rand = arc4random();
						byte = (rand&0xff000000) >> 24;
						break;
					case 1:
						byte = (rand&0x00ff0000) >> 16;
						break;
					case 2:
						byte = (rand&0x0000ff00) >> 8;
						break;
					case 3:
						byte = (rand&0x000000ff);
						i = 0;
						break;
					}
					if (byte < 60) {
						AG_TransformRotate(r, 0);
					} else if (byte < 120) {
						AG_TransformRotate(r, 90);
					} else if (byte < 180) {
						AG_TransformRotate(r, 180);
					} else if (byte < 240) {
						AG_TransformRotate(r, 270);
					}
				}
#endif
			}
		}
		break;
	case FILL_CLEAR:
		for (y = dy; y < dy+dh; y++) {
			for (x = dx; x < dx+dw; x++) {
				MAP_modNodeChg(m, x, y);
				MAP_NodeRemoveAll(m, &m->map[y][x],
				    m->cur_layer);
			}
		}
		break;
	}
	MAP_modEnd(m);
	return (1);
}

const MAP_ToolOps agMapFillOps = {
	"Fill", N_("Clear/fill layer"),
	FILL_TOOL_ICON,
	sizeof(struct rg_fill_tool),
	0,
	init,
	NULL,			/* destroy */
	pane,
	NULL,			/* edit */
	NULL,			/* cursor */
	effect,
	
	NULL,			/* mousemotion */
	NULL,			/* mousebuttondown */
	NULL,			/* mousebuttonup */
	NULL,			/* keydown */
	NULL,			/* keyup */
};