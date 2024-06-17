/*
Copyright (C) 1996-1997 Id Software, Inc.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

// world.c -- world query functions

#include "qwsvdef.h"

/*

Entities never clip against themselves, or their owner

Line of sight checks trace->crosscontent, but bullets don't

*/

typedef struct
{
    vec3_t boxmins, boxmaxs; // Enclose the test object along entire move
    float *mins, *maxs;      // Size of the moving object
    vec3_t mins2, maxs2;     // Size when clipping against monsters
    float *start, *end;
    trace_t trace;
    int type;
    edict_t *passedict;
} moveclip_t;

int SV_HullPointContents(hull_t *hull, int num, vec3_t p);

// Hull Boxes
static hull_t box_hull;
static dclipnode_t box_clipnodes[6];
static mplane_t box_planes[6];

/*
===================
SV_InitBoxHull

Set up the planes and clipnodes so that the six sides of a bounding box
can be stored and get a proper hull_t structure.
===================
*/
void SV_InitBoxHull(void)
{
    int i;
    int side;

    box_hull.clipnodes = box_clipnodes;
    box_hull.planes = box_planes;
    box_hull.firstclipnode = 0;
    box_hull.lastclipnode = 5;

    for (i = 0; i < 6; i++)
    {
        box_clipnodes[i].planenum = i;
        
        side = i & 1;
        
        box_clipnodes[i].children[side] = CONTENTS_EMPTY;
        if (i != 5)
            box_clipnodes[i].children[side ^ 1] = i + 1;
        else
            box_clipnodes[i].children[side ^ 1] = CONTENTS_SOLID;
        
        box_planes[i].type = i >> 1;
        box_planes[i].normal[i >> 1] = 1;
    }
}

/*
===================
SV_HullForBox

Convert bounding boxes into small BSP trees instead of comparing directly.
===================
*/
hull_t *SV_HullForBox(vec3_t mins, vec3_t maxs)
{
    box_planes[0].dist = maxs[0];
    box_planes[1].dist = mins[0];
    box_planes[2].dist = maxs[1];
    box_planes[3].dist = mins[1];
    box_planes[4].dist = maxs[2];
    box_planes[5].dist = mins[2];

    return &box_hull;
}

/*
================
SV_HullForEntity

Returns a hull that can be used for testing or clipping an object of mins/maxs size.
Offset is filled in to contain the adjustment that must be added to the
testing object's origin to get a point to use with the returned hull.
================
*/
hull_t *SV_HullForEntity(edict_t *ent, vec3_t mins, vec3_t maxs, vec3_t offset)
{
    model_t *model;
    vec3_t size;
    vec3_t hullmins, hullmaxs;
    hull_t *hull;

    // Decide which clipping hull to use, based on the size
    if (ent->v.solid == SOLID_BSP)
    {
        // Explicit hulls in the BSP model
        if (ent->v.movetype != MOVETYPE_PUSH)
            SV_Error("SOLID_BSP without MOVETYPE_PUSH");

        model = sv.models[(int)ent->v.modelindex];

        if (!model || model->type != mod_brush)
            SV_Error("MOVETYPE_PUSH with a non-bsp model");

        VectorSubtract(maxs, mins, size);
        if (size[0] < 3)
            hull = &model->hulls[0];
        else if (size[0] <= 32)
            hull = &model->hulls[1];
        else
            hull = &model->hulls[2];

        // Calculate an offset value to center the origin
        VectorSubtract(hull->clip_mins, mins, offset);
        VectorAdd(offset, ent->v.origin, offset);
    }
    else
    {
        // Create a temporary hull from bounding box sizes
        VectorSubtract(ent->v.mins, maxs, hullmins);
        VectorSubtract(ent->v.maxs, mins, hullmaxs);
        hull = SV_HullForBox(hullmins, hullmaxs);
        VectorCopy(ent->v.origin, offset);
    }

    return hull;
}

// Entity Area Checking
areanode_t sv_areanodes[AREA_NODES];
int sv_numareanodes;

/*
===============
SV_CreateAreaNode
===============
*/
areanode_t *SV_CreateAreaNode(int depth, vec3_t mins, vec3_t maxs)
{
    areanode_t *anode;
    vec3_t size;
    vec3_t mins1, maxs1, mins2, maxs2;

    anode = &sv_areanodes[sv_numareanodes];
    sv_numareanodes++;

    ClearLink(&anode->trigger_edicts);
    ClearLink(&anode->solid_edicts);
    
    if (depth == AREA_DEPTH)
    {
        anode->axis = -1;
        anode->children[0] = anode->children[1] = NULL;
        return anode;
    }
    
    VectorSubtract(maxs, mins, size);
    if (size[0] > size[1])
        anode->axis = 0;
    else
        anode->axis = 1;
    
    anode->dist = 0.5 * (maxs[anode->axis] + mins[anode->axis]);
    VectorCopy(mins, mins1);    
    VectorCopy(mins, mins2);    
    VectorCopy(maxs, maxs1);    
    VectorCopy(maxs, maxs2);    
    
    maxs1[anode->axis] = mins2[anode->axis] = anode->dist;
    
    anode->children[0] = SV_CreateAreaNode(depth + 1, mins2, maxs2);
    anode->children[1] = SV_CreateAreaNode(depth + 1, mins1, maxs1);

    return anode;
}

/*
===============
SV_ClearWorld
===============
*/
void SV_ClearWorld(void)
{
    SV_InitBoxHull();
    
    memset(sv_areanodes, 0, sizeof(sv_areanodes));
    sv_numareanodes = 0;
    SV_CreateAreaNode(0, sv.worldmodel->mins, sv.worldmodel->maxs);
}

/*
===============
SV_UnlinkEdict
===============
*/
void SV_UnlinkEdict(edict_t *ent)
{
    if (!ent->area.prev)
        return; // Not linked anywhere
    RemoveLink(&ent->area);
    ent->area.prev = ent->area.next = NULL;
}

/*
====================
SV_TouchLinks
====================
*/
void SV_TouchLinks(edict_t *ent, areanode_t *node)
{
    link_t *l, *next;
    edict_t *touch;
    int old_self, old_other;

    // Touch linked edicts
    for (l = node->trigger_edicts.next; l != &node->trigger_edicts; l = next)
    {
        next = l->next;
        touch = EDICT_FROM_AREA(l);
        if (touch == ent)
            continue;
        if (!touch->v.touch || touch->v.solid != SOLID_TRIGGER)
            continue;
        if (ent->v.absmin[0] > touch->v.absmax[0] ||
            ent->v.absmin[1] > touch->v.absmax[1] ||
            ent->v.absmin[2] > touch->v.absmax[2] ||
            ent->v.absmax[0] < touch->v.absmin[0] ||
            ent->v.absmax[1] < touch->v.absmin[1] ||
            ent->v.absmax[2] < touch->v.absmin[2])
            continue;

        old_self = pr_global_struct->self;
        old_other = pr_global_struct->other;

        pr_global_struct->self = EDICT_TO_PROG(touch);
        pr_global_struct->other = EDICT_TO_PROG(ent);
        pr_global_struct->time = sv.time;
        PR_ExecuteProgram(touch->v.touch);

        pr_global_struct->self = old_self;
        pr_global_struct->other = old_other;
    }

    // Recurse down both sides
    if (node->axis == -1)
        return;
    
    if (ent->v.absmax[node->axis] > node->dist)
        SV_TouchLinks(ent, node->children[0]);
    if (ent->v.absmin[node->axis] < node->dist)
        SV_TouchLinks(ent, node->children[1]);
}

/*
===============
SV_FindTouchedLeafs
===============
*/
void SV_FindTouchedLeafs(edict_t *ent, mnode_t *node)
{
    mplane_t *splitplane;
    mleaf_t *leaf;
    int sides;
    int leafnum;

    if (node->contents == CONTENTS_SOLID)
        return;

    // Add an efrag if the node is a leaf
    if (node->contents < 0)
    {
        if (ent->num_leafs == MAX_ENT_LEAFS)
            return;

        leaf = (mleaf_t *)node;
        leafnum = leaf - sv.worldmodel->leafs - 1;

        ent->leafnums[ent->num_leafs] = leafnum;
        ent->num_leafs++;
        return;
    }

    // NODE_MIXED
    splitplane = node->plane;
    sides = BOX_ON_PLANE_SIDE(ent->v.absmin, ent->v.absmax, splitplane);

    // Recurse down the contacted sides
    if (sides & 1)
        SV_FindTouchedLeafs(ent, node->children[0]);
    if (sides & 2)
        SV_FindTouchedLeafs(ent, node->children[1]);
}

/*
===============
SV_LinkEdict
===============
*/
void SV_LinkEdict(edict_t *ent, qboolean touch_triggers)
{
    areanode_t *node;
    
    if (ent->area.prev)
        SV_UnlinkEdict(ent); // Unlink from old position

    if (ent == sv.edicts)
        return; // Don't add the world

    if (ent->free)
        return;

    // Set the abs box
    VectorAdd(ent->v.origin, ent->v.mins, ent->v.absmin);
    VectorAdd(ent->v.origin, ent->v.maxs, ent->v.absmax);

    // To make items easier to pick up and allow them to be grabbed off
    // of shelves, the abs sizes are expanded
    if ((int)ent->v.flags & FL_ITEM)
    {
        ent->v.absmin[0] -= 15;
        ent->v.absmin[1] -= 15;
        ent->v.absmax[0] += 15;
        ent->v.absmax[1] += 15;
    }
    else
    {
        // Because movement is clipped an epsilon away from an actual edge,
        // we must fully check even when bounding boxes don't quite touch
        ent->v.absmin[0] -= 1;
        ent->v.absmin[1] -= 1;
        ent->v.absmin[2] -= 1;
        ent->v.absmax[0] += 1;
        ent->v.absmax[1] += 1;
        ent->v.absmax[2] += 1;
    }

    // Link to PVS leafs
    ent->num_leafs = 0;
    if (ent->v.modelindex)
        SV_FindTouchedLeafs(ent, sv.worldmodel->nodes);

    if (ent->v.solid == SOLID_NOT)
        return;

    // Find the first node that the ent's box crosses
    node = sv_areanodes;
    while (1)
    {
        if (node->axis == -1)
            break;
        if (ent->v.absmin[node->axis] > node->dist)
            node = node->children[0];
        else if (ent->v.absmax[node->axis] < node->dist)
            node = node->children[1];
        else
            break; // Crosses the node
    }

    // Link it in
    if (ent->v.solid == SOLID_TRIGGER)
        InsertLinkBefore(&ent->area, &node->trigger_edicts);
    else
        InsertLinkBefore(&ent->area, &node->solid_edicts);

    // If touch_triggers, touch all entities at this node and descend for more
    if (touch_triggers)
        SV_TouchLinks(ent, sv_areanodes);
}

/*
===============================================================================

POINT TESTING IN HULLS

===============================================================================
*/

#if !id386

/*
==================
SV_HullPointContents

==================
*/
int SV_HullPointContents(hull_t *hull, int num, vec3_t p)
{
    float d;
    dclipnode_t *node;
    mplane_t *plane;

    while (num >= 0)
    {
        if (num < hull->firstclipnode || num > hull->lastclipnode)
            SV_Error("SV_HullPointContents: bad node number");

        node = hull->clipnodes + num;
        plane = hull->planes + node->planenum;

        if (plane->type < 3)
            d = p[plane->type] - plane->dist;
        else
            d = DotProduct(plane->normal, p) - plane->dist;
        if (d < 0)
            num = node->children[1];
        else
            num = node->children[0];
    }

    return num;
}

#endif // !id386

/*
==================
SV_PointContents
==================
*/
int SV_PointContents(vec3_t p)
{
    return SV_HullPointContents(&sv.worldmodel->hulls[0], 0, p);
}

// Line Testing in Hulls
#define DIST_EPSILON (0.03125)

/*
==================
SV_RecursiveHullCheck
==================
*/
qboolean SV_RecursiveHullCheck(hull_t *hull, int num, float p1f, float p2f, vec3_t p1, vec3_t p2, trace_t *trace)
{
    dclipnode_t *node;
    mplane_t *plane;
    float t1, t2;
    float frac;
    int i;
    vec3_t mid;
    int side;
    float midf;

    // Check for empty
    if (num < 0)
    {
        if (num != CONTENTS_SOLID)
        {
            trace->allsolid = false;
            if (num == CONTENTS_EMPTY)
                trace->inopen = true;
            else
                trace->inwater = true;
        }
        else
            trace->startsolid = true;
        return true; // Empty
    }

    if (num < hull->firstclipnode || num > hull->lastclipnode)
        SV_Error("SV_RecursiveHullCheck: bad node number");

    // Find the point distances
    node = hull->clipnodes + num;
    plane = hull->planes + node->planenum;

    if (plane->type < 3)
    {
        t1 = p1[plane->type] - plane->dist;
        t2 = p2[plane->type] - plane->dist;
    }
    else
    {
        t1 = DotProduct(plane->normal, p1) - plane->dist;
        t2 = DotProduct(plane->normal, p2) - plane->dist;
    }

    if (t1 >= 0 && t2 >= 0)
        return SV_RecursiveHullCheck(hull, node->children[0], p1f, p2f, p1, p2, trace);
    if (t1 < 0 && t2 < 0)
        return SV_RecursiveHullCheck(hull, node->children[1], p1f, p2f, p1, p2, trace);

    // Put the crosspoint DIST_EPSILON pixels on the near side
    if (t1 < 0)
        frac = (t1 + DIST_EPSILON) / (t1 - t2);
    else
        frac = (t1 - DIST_EPSILON) / (t1 - t2);
    if (frac < 0)
        frac = 0;
    if (frac > 1)
        frac = 1;

    midf = p1f + (p2f - p1f) * frac;
    for (i = 0; i < 3; i++)
        mid[i] = p1[i] + frac * (p2[i] - p1[i]);

    side = (t1 < 0);

    // Move up to the node
    if (!SV_RecursiveHullCheck(hull, node->children[side], p1f, midf, p1, mid, trace))
        return false;

    if (SV_HullPointContents(hull, node->children[side ^ 1], mid) != CONTENTS_SOLID)
        // Go past the node
        return SV_RecursiveHullCheck(hull, node->children[side ^ 1], midf, p2f, mid, p2, trace);

    if (trace->allsolid)
        return false; // Never got out of the solid area

    // The other side of the node is solid, this is the impact point
    if (!side)
    {
        VectorCopy(plane->normal, trace->plane.normal);
        trace->plane.dist = plane->dist;
    }
    else
    {
        VectorSubtract(vec3_origin, plane->normal, trace->plane.normal);
        trace->plane.dist = -plane->dist;
    }

    while (SV_HullPointContents(hull, hull->firstclipnode, mid) == CONTENTS_SOLID)
    {
        // Shouldn't really happen, but does occasionally
        frac -= 0.1;
        if (frac < 0)
        {
            trace->fraction = midf;
            VectorCopy(mid, trace->endpos);
            Con_Printf("Backup past 0\n");
            return false;
        }
        midf = p1f + (p2f - p1f) * frac;
        for (i = 0; i < 3; i++)
            mid[i] = p1[i] + frac * (p2[i] - p1[i]);
    }

    trace->fraction = midf;
    VectorCopy(mid, trace->endpos);

    return false;
}

/*
==================
SV_ClipMoveToEntity

Handles selection or creation of a clipping hull, and offsetting (and
eventually rotating) of the end points
==================
*/
trace_t SV_ClipMoveToEntity(edict_t *ent, vec3_t start, vec3_t mins, vec3_t maxs, vec3_t end)
{
    trace_t trace;
    vec3_t offset;
    vec3_t start_l, end_l;
    hull_t *hull;

    // Fill in a default trace
    memset(&trace, 0, sizeof(trace_t));
    trace.fraction = 1;
    trace.allsolid = true;
    VectorCopy(end, trace.endpos);

    // Get the clipping hull
    hull = SV_HullForEntity(ent, mins, maxs, offset);

    VectorSubtract(start, offset, start_l);
    VectorSubtract(end, offset, end_l);

    // Trace a line through the appropriate clipping hull
    SV_RecursiveHullCheck(hull, hull->firstclipnode, 0, 1, start_l, end_l, &trace);

    // Fix trace up by the offset
    if (trace.fraction != 1)
        VectorAdd(trace.endpos, offset, trace.endpos);

    // Did we clip the move?
    if (trace.fraction < 1 || trace.startsolid)
        trace.ent = ent;

    return trace;
}

/*
====================
SV_ClipToLinks

Mins and maxs enclose the entire area swept by the move
====================
*/
void SV_ClipToLinks(areanode_t *node, moveclip_t *clip)
{
    link_t *l, *next;
    edict_t *touch;
    trace_t trace;

    // Touch linked edicts
    for (l = node->solid_edicts.next; l != &node->solid_edicts; l = next)
    {
        next = l->next;
        touch = EDICT_FROM_AREA(l);
        if (touch->v.solid == SOLID_NOT)
            continue;
        if (touch == clip->passedict)
            continue;
        if (touch->v.solid == SOLID_TRIGGER)
            SV_Error("Trigger in clipping list");

        if (clip->type == MOVE_NOMONSTERS && touch->v.solid != SOLID_BSP)
            continue;

        if (clip->boxmins[0] > touch->v.absmax[0] ||
            clip->boxmins[1] > touch->v.absmax[1] ||
            clip->boxmins[2] > touch->v.absmax[2] ||
            clip->boxmaxs[0] < touch->v.absmin[0] ||
            clip->boxmaxs[1] < touch->v.absmin[1] ||
            clip->boxmaxs[2] < touch->v.absmin[2])
            continue;

        if (clip->passedict && clip->passedict->v.size[0] && !touch->v.size[0])
            continue; // Points never interact

        // Might intersect, so do an exact clip
        if (clip->trace.allsolid)
            return;
        if (clip->passedict)
        {
            if (PROG_TO_EDICT(touch->v.owner) == clip->passedict)
                continue; // Don't clip against own missiles
            if (PROG_TO_EDICT(clip->passedict->v.owner) == touch)
                continue; // Don't clip against owner
        }

        if ((int)touch->v.flags & FL_MONSTER)
            trace = SV_ClipMoveToEntity(touch, clip->start, clip->mins2, clip->maxs2, clip->end);
        else
            trace = SV_ClipMoveToEntity(touch, clip->start, clip->mins, clip->maxs, clip->end);
        if (trace.allsolid || trace.startsolid ||
            trace.fraction < clip->trace.fraction)
        {
            trace.ent = touch;
            if (clip->trace.startsolid)
            {
                clip->trace = trace;
                clip->trace.startsolid = true;
            }
            else
                clip->trace = trace;
        }
        else if (trace.startsolid)
            clip->trace.startsolid = true;
    }

    // Recurse down both sides
    if (node->axis == -1)
        return;

    if (clip->boxmaxs[node->axis] > node->dist)
        SV_ClipToLinks(node->children[0], clip);
    if (clip->boxmins[node->axis] < node->dist)
        SV_ClipToLinks(node->children[1], clip);
}

/*
==================
SV_MoveBounds
==================
*/
void SV_MoveBounds(vec3_t start, vec3_t mins, vec3_t maxs, vec3_t end, vec3_t boxmins, vec3_t boxmaxs)
{
    int i;

    for (i = 0; i < 3; i++)
    {
        if (end[i] > start[i])
        {
            boxmins[i] = start[i] + mins[i] - 1;
            boxmaxs[i] = end[i] + maxs[i] + 1;
        }
        else
        {
            boxmins[i] = end[i] + mins[i] - 1;
            boxmaxs[i] = start[i] + maxs[i] + 1;
        }
    }
}

/*
==================
SV_Move
==================
*/
trace_t SV_Move(vec3_t start, vec3_t mins, vec3_t maxs, vec3_t end, int type, edict_t *passedict)
{
    moveclip_t clip;
    int i;

    memset(&clip, 0, sizeof(moveclip_t));

    // Clip to world
    clip.trace = SV_ClipMoveToEntity(sv.edicts, start, mins, maxs, end);

    clip.start = start;
    clip.end = end;
    clip.mins = mins;
    clip.maxs = maxs;
    clip.type = type;
    clip.passedict = passedict;

    if (type == MOVE_MISSILE)
    {
        for (i = 0; i < 3; i++)
        {
            clip.mins2[i] = -15;
            clip.maxs2[i] = 15;
        }
    }
    else
    {
        VectorCopy(mins, clip.mins2);
        VectorCopy(maxs, clip.maxs2);
    }

    // Create the bounding box of the entire move
    SV_MoveBounds(start, clip.mins2, clip.maxs2, end, clip.boxmins, clip.boxmaxs);

    // Clip to entities
    SV_ClipToLinks(sv_areanodes, &clip);

    return clip.trace;
}

// Test Player Position
edict_t *SV_TestPlayerPosition(edict_t *ent, vec3_t origin)
{
    hull_t *hull;
    edict_t *check;
    vec3_t boxmins, boxmaxs;
    vec3_t offset;
    int e;

    // Check world first
    hull = &sv.worldmodel->hulls[1];
    if (SV_HullPointContents(hull, hull->firstclipnode, origin) != CONTENTS_EMPTY)
        return sv.edicts;

    // Check all entities
    VectorAdd(origin, ent->v.mins, boxmins);
    VectorAdd(origin, ent->v.maxs, boxmaxs);

    check = NEXT_EDICT(sv.edicts);
    for (e = 1; e < sv.num_edicts; e++, check = NEXT_EDICT(check))
    {
        if (check->free)
            continue;
        if (check->v.solid != SOLID_BSP &&
            check->v.solid != SOLID_BBOX &&
            check->v.solid != SOLID_SLIDEBOX)
            continue;

        if (boxmins[0] > check->v.absmax[0] ||
            boxmins[1] > check->v.absmax[1] ||
            boxmins[2] > check->v.absmax[2] ||
            boxmaxs[0] < check->v.absmin[0] ||
            boxmaxs[1] < check->v.absmin[1] ||
            boxmaxs[2] < check->v.absmin[2])
            continue;

        if (check == ent)
            continue;

        // Get the clipping hull
        hull = SV_HullForEntity(check, ent->v.mins, ent->v.maxs, offset);

        VectorSubtract(origin, offset, offset);

        // Test the point
        if (SV_HullPointContents(hull, hull->firstclipnode, offset) != CONTENTS_EMPTY)
            return check;
    }

    return NULL;
}


