// Quick Dumper : Moves items marked as "dump" to cursor

#include <iostream>
#include <iomanip>
#include <sstream>
#include <climits>
#include <vector>
#include <set>
using namespace std;

#include <DFHack.h>
#include <dfhack/DFVector.h>
#include <dfhack/extra/MapExtras.h> // map cache for the win.
using namespace DFHack;
using MapExtras::Block;
using MapExtras::MapCache;

typedef std::map <DFCoord, uint32_t> coordmap;

int main ()
{

    DFHack::ContextManager CM ("Memory.xml");
    DFHack::Context * DF;
    DFHack::VersionInfo *mem;
    DFHack::Gui * Gui;
    DFHack::Items * Items;
    DFHack::Maps *Maps;
    DFHack::occupancies40d * occupancies;
    
    cout << "This utility lets you quickly move all items designated to be dumped." << endl
         << "Items are instantly moved to the cursor position, the dump flag is unset," << endl
         << "and the forbid flag is set, as if it had been dumped normally." << endl 
         << "Be aware that any active dump item tasks still point at the item." << endl << endl;
    try
    {
        DF = CM.getSingleContext();
        DF->Attach();
        mem = DF->getMemoryInfo();
        Gui = DF->getGui();
        Items = DF->getItems();
        Maps = DF->getMaps();
    }
    catch (exception& e)
    {
        cerr << e.what() << endl;
        #ifndef LINUX_BUILD
            cin.ignore();
        #endif
        return 1;
    }

    DFHack::Process * p = DF->getProcess();

    // FIXME: these can fail and should be wrapped in a try-catch
    DFHack::OffsetGroup* itemGroup = mem->getGroup("Items");
    unsigned vector_addr = itemGroup->getAddress("items_vector");
    DFHack::DfVector <uint32_t> p_items (p, vector_addr);

    uint32_t numItems = p_items.size();

    // init the map
    if(!Maps->Start())
    {
        cerr << "Can't initialize map." << endl;
        #ifndef LINUX_BUILD
            cin.ignore();
        #endif
        return 1;
    }
    MapCache MC (Maps);

    int i = 0;
    int dumped_total = 0;

    int cx, cy, cz;

    if (!Gui->getCursorCoords(cx,cy,cz))
    {
        cerr << "Cursor position not found.  Please enabled the cursor." << endl;
        #ifndef LINUX_BUILD
            cin.ignore();
        #endif
        return 1;
    }
    DFCoord pos_cursor(cx,cy,cz);
    {
        Block * b = MC.BlockAt(pos_cursor / 16);
        if(!b)
        {
            cerr << "Cursor is in an invalid area. Place it over something save first." << endl;
            #ifndef LINUX_BUILD
                cin.ignore();
            #endif
            return 1;
        }
        // TODO: check if the target is floor? maybe?
    }
    coordmap counts;
    // proceed with the dumpification operation
    for(uint32_t i=0; i< numItems; i++)
    {
        DFHack::dfh_item temp;
        Items->readItem(p_items[i],temp);
        DFCoord pos_item(temp.base.x, temp.base.y, temp.base.z);

        // keep track how many items are at places. all items.
        coordmap::iterator it = counts.find(pos_item);
        if(it == counts.end())
        {
            std::pair< coordmap::iterator, bool > inserted = counts.insert(std::make_pair(pos_item,1));
            it = inserted.first;
        }
        else
        {
            it->second ++;
        }
        // iterator is valid here, we use it later to decrement the pile counter if the item is moved

        // only dump the stuff marked for dumping and laying on the ground
        if (!temp.base.flags.dump || !temp.base.flags.on_ground)
            continue;

        // Change flags to indicate the dump was completed, as if by super-dwarfs
        temp.base.flags.dump = 0;
        temp.base.flags.forbid = 1;

        // Don't move items if they're already at the cursor
        if (pos_cursor == pos_item)
            continue;

        // Move the item
        temp.base.x = pos_cursor.x;
        temp.base.y = pos_cursor.y;
        temp.base.z = pos_cursor.z;
        Items->writeItem(temp);
        // keeping track of item pile sizes ;)
        it->second --;
        dumped_total++;
    }
    // for each item pile, see if it reached zero. if so, unset item flag on the tile it's on
    coordmap::iterator it = counts.begin();
    coordmap::iterator end = counts.end();
    while(it != end)
    {
        if(it->second == 0)
        {
            t_occupancy occ = MC.occupancyAt(it->first);
            occ.bits.item = false;
            MC.setOccupancyAt(it->first, occ);
        }
        it++;
    }
    // Set "item here" flag on target tile, if we moved any items to the target tile.
    if (dumped_total > 0)
    {
        // assume there is a possibility the cursor points at some weird location with missing block data
        Block * b = MC.BlockAt(pos_cursor / 16);
        if(b)
        {
            t_occupancy occ = MC.occupancyAt(pos_cursor);
            occ.bits.item = 1;
            MC.setOccupancyAt(pos_cursor,occ);
        }
    }
    // write map changes back to DF.
    MC.WriteAll();
    // Is this necessary?  Is "forbid" a dirtyable attribute like "dig" is?
    Maps->WriteDirtyBit(cx/16, cy/16, cz, true);

    DF->Detach();
    cout << "Done.  " << dumped_total << " items quickdumped." << endl;
#ifndef LINUX_BUILD
    cout << "Press any key to continue" << endl;
    cin.ignore();
#endif
    return 0;
}