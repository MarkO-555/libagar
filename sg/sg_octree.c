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
#include "sg.h"

void
SG_OctreeInit(SG *sg, SG_Octree *oct)
{
	oct->root = NULL;
}

int
SG_OctreeBuild(SG *sg, SG_Octree *oct)
{
	SG_Object *so;
	SG_Facet *fct;
	SG_Vector min = SG_0;
	SG_Vector max = SG_0;
	int i;

	/* Find the scene extrema. */
	SG_FOREACH_NODE_CLASS(so, sg, sg_object, "Object:*") {
		for (i = 0; i < so->nfacets; i++) {
			SG_Polygon P = SG_FacetPolygon(so, i);
			int i;

			for (i = 0; i < P.n; i++) {
				if (P.v[i].x < min.x) { min.x = P.v[i].x; }
				if (P.v[i].y < min.y) { min.y = P.v[i].y; }
				if (P.v[i].z < min.z) { min.z = P.v[i].z; }
				if (P.v[i].x > max.x) { max.x = P.v[i].x; }
				if (P.v[i].y > max.y) { max.y = P.v[i].y; }
				if (P.v[i].z > max.z) { max.z = P.v[i].z; }
			}
		}
	}

	oct->root = Malloc(sizeof(SG_Octnode), M_SG);
}

int
SG_OctreeBuildNode(SG *sg, SG_Octree *oct, SG_Vector min, SG_Vector max)
{
	SG_Object *so;
	SG_Facet *fct;
	int i;

	SG_FOREACH_NODE_CLASS(so, sg, sg_object, "Object:*") {
		for (i = 0; i < so->nfacets; i++) {
			SG_Polygon P = SG_FacetPolygon(so, i);

			
		}
	}
}