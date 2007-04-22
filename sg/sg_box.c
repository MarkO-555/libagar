/*
 * Copyright (c) 2006-2007 Hypertriton, Inc.
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
#include <agar/gui/gui.h>

#include "sg.h"
#include "sg_gui.h"

SG_Box *
SG_BoxNew(void *pnode, const char *name)
{
	SG_Box *box;

	box = Malloc(sizeof(SG_Box), M_SG);
	SG_BoxInit(box, name);
	SG_NodeAttach(pnode, box);
	return (box);
}

void
SG_BoxInit(void *p, const char *name)
{
	SG_Box *box = p;

	SG_ObjectInit(box, name);
	SGNODE(box)->ops = &sgBoxOps;

	SG_EdgeRehash(box, 8);
	SG_VertexNew(box, SG_0);
	SG_VertexNew(box, SG_VECTOR(-0.5, -1.0, -0.5));
	SG_VertexNew(box, SG_VECTOR(+0.5, -1.0, -0.5));
	SG_VertexNew(box, SG_VECTOR(+0.5, -1.0, +0.5));
	SG_VertexNew(box, SG_VECTOR(-0.5, -1.0, +0.5));
	SG_FacetFromTri3(box, 1,3,2);
	SG_FacetFromTri3(box, 4,3,1);
	SG_FacetFromTri3(box, 1,5,4);
	SG_FacetFromTri3(box, 1,2,5);
	SG_FacetFromQuad4(box, 2,3,4,5);
	OBJ_N(box,1) = SG_J;
	OBJ_N(box,2) = SG_INV(SG_J);
	OBJ_N(box,3) = SG_INV(SG_J);
	OBJ_N(box,4) = SG_INV(SG_J);
	OBJ_N(box,5) = SG_INV(SG_J);
}

int
SG_BoxLoad(void *p, AG_Netbuf *buf)
{
	SG_Box *box = p;

	return (SG_ObjectLoad(box, buf));
}

int
SG_BoxSave(void *p, AG_Netbuf *buf)
{
	SG_Box *box = p;

	return (SG_ObjectSave(box, buf));
}

SG_NodeOps sgBoxOps = {
	"Object:Box",
	sizeof(SG_Box),
	0,
	SG_BoxInit,
	NULL,			/* destroy */
	SG_BoxLoad,
	SG_BoxSave,
	NULL,			/* edit */
	SG_ObjectMenuInstance,
	NULL,			/* menuClass */
	SG_ObjectDraw
};