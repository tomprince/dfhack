/*
www.sourceforge.net/projects/dfhack
Copyright (c) 2009 Petr Mrázek (peterix), Kenneth Ferland (Impaler[WrG]), dorf

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any
damages arising from the use of this software.

Permission is granted to anyone to use this software for any
purpose, including commercial applications, and to alter it and
redistribute it freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must
not claim that you wrote the original software. If you use this
software in a product, an acknowledgment in the product documentation
would be appreciated but is not required.

2. Altered source versions must be plainly marked as such, and
must not be misrepresented as being the original software.

3. This notice may not be removed or altered from any source
distribution.
*/

#include "Internal.h"

#include <string>
#include <sstream>
#include <vector>
#include <cstdio>
#include <map>
using namespace std;

#include "ContextShared.h"
#include "dfhack/DFTypes.h"
#include "dfhack/VersionInfo.h"
#include "dfhack/DFProcess.h"
#include "dfhack/DFVector.h"
#include "dfhack/modules/Materials.h"
#include "dfhack/modules/Items.h"
#include "ModuleFactory.h"

using namespace DFHack;

Module* DFHack::createItems(DFContextShared * d)
{
    return new Items(d);
}

/* this is used to store data about the way accessors work */
class Accessor
{
private:
    uint32_t method;
public:
    Accessor(uint32_t method);
    virtual std::string dump() = 0;
    virtual int32_t getValue(uint32_t objectPtr) = 0;
    virtual bool isConstant();
    virtual ~Accessor();
};

Accessor::Accessor(uint32_t method)
        : method(method)
{
}

Accessor::~Accessor()
{
}

std::string Accessor::dump()
{
    stringstream sstr;
    sstr << hex << "method @0x" << method;
    return sstr.str();
}

bool Accessor::isConstant()
{
    return false;
}

template <typename T>
class IndirectAccessor : public Accessor
{
private:
    Process* p;
    std::vector<uint32_t> offsets;
public:
    template <typename... A>
    IndirectAccessor(uint32_t method, Process* p, A... offsets);
    virtual ~IndirectAccessor();
    virtual std::string dump();
    virtual int32_t getValue(uint32_t objectPtr);
};

template <typename T>
template <typename... A>
IndirectAccessor<T>::IndirectAccessor(uint32_t method, Process* p, A... offsets)
	: Accessor(method), p(p), offsets{offsets...}
{
}

template <typename T>
IndirectAccessor<T>::~IndirectAccessor()
{
}

template <typename T>
std::string IndirectAccessor<T>::dump()
{
    stringstream sstr;
    sstr << Accessor::dump() << " ";
    // FIXME: Size
    for (size_t i = 0; i < offsets.size(); ++i) {
         sstr << "[ ";
    }
    sstr << "obj +" << hex;
    for (size_t i = 0; i < offsets.size(); ++i) {
        sstr << " 0x" << offsets[i] << " ]";
    }
    return sstr.str();
}

template <typename T>
int32_t IndirectAccessor<T>::getValue(uint32_t objectPtr)
{
    for (size_t i = 0; i < offsets.size() - 1; ++i) {
            objectPtr = p->readDWord(objectPtr + offsets[i]);
    }
    return p->read<T>(objectPtr + offsets[offsets.size()]);
}

class ConstantAccessor : public Accessor
{
private:
    uint32_t constant;
public:
    ConstantAccessor(uint32_t method, uint32_t contant);
    virtual ~ConstantAccessor();
    virtual std::string dump();
    virtual bool isConstant();
    virtual int32_t getValue(uint32_t objectPtr);
};

ConstantAccessor::ConstantAccessor(uint32_t method, uint32_t constant)
    : Accessor(method), constant(constant)
{
}

ConstantAccessor::~ConstantAccessor()
{
}

std::string ConstantAccessor::dump()
{
    stringstream sstr;
    sstr << Accessor::dump() << " Constant: " << dec << constant;
    return sstr.str();
}

int32_t ConstantAccessor::getValue(uint32_t objectPtr)
{
    return constant;
}

bool ConstantAccessor::isConstant()
{
    return true;
}

enum DataWidth {
    Data32 = 0,
    DataSigned16,
    DataUnsigned16
};

class DFHACK_EXPORT ItemImprovementDesc
{
private:
    Accessor * AType;
    Process * p;
public:
    ItemImprovementDesc(uint32_t VTable, Process * p);
    bool getImprovement(uint32_t descptr, t_improvement & imp);
    uint32_t vtable;
    uint32_t maintype;
};

class DFHACK_EXPORT ItemDesc
{
private:
    Accessor * AMainType;
    Accessor * ASubType;
    Accessor * ASubIndex;
    Accessor * AIndex;
    Accessor * AQuality;
    Accessor * AWear;
    Process * p;
    bool hasDecoration;
public:
    ItemDesc(uint32_t VTable, Process * p);
    bool readItem(uint32_t itemptr, dfh_item & item);
    std::string dumpAccessors();
    std::string className;
    uint32_t vtable;
    uint32_t mainType;
    std::vector<ItemImprovementDesc> improvement;
};

inline bool do_match(uint32_t &ptr, uint64_t val, int size, uint64_t mask, uint64_t check)
{
    if ((val & mask) == check) {
        ptr += size;
        return true;
    }
    return false;
}

static bool match_MEM_ACCESS(uint32_t &ptr, uint64_t v, int isize, int in_reg, int &out_reg, uint32_t &offset)
{
    // ESP & EBP are hairy
    if (in_reg == 4 || in_reg == 5)
        return false;

    if ((v & 7) != in_reg)
        return false;

    out_reg = (v>>3) & 7;

    switch ((v>>6)&3) {
        case 0: // MOV REG2, [REG]
            offset = 0;
            ptr += isize+1;
            return true;
        case 1: // MOV REG2, [REG+offset8]
            offset = (signed char)(v >> 8);
            ptr += isize+2;
            return true;
        case 2: // MOV REG2, [REG+offset32]
            offset = (signed int)(v >> 8);
            ptr += isize+5;
            return true;
        default:
            return false;
    }
}

static bool match_MOV_MEM(uint32_t &ptr, uint64_t v, int in_reg, int &out_reg, uint32_t &offset, DataWidth &size)
{
    int prefix = 0;
    size = Data32;
    if ((v & 0xFF) == 0x8B) { // MOV
        v >>= 8;
        prefix = 1;
    }
    else if ((v & 0xFFFF) == 0x8B66) { // MOV 16-bit
        v >>= 16;
        prefix = 2;
        size = DataUnsigned16;
    }
    else if ((v & 0xFFFF) == 0xBF0F) { // MOVSX
        v >>= 16;
        prefix = 2;
        size = DataSigned16;
    }
    else if ((v & 0xFFFF) == 0xB70F) { // MOVZ
        v >>= 16;
        prefix = 2;
        size = DataUnsigned16;
    }
    else
        return false;

    return match_MEM_ACCESS(ptr, v, prefix, in_reg, out_reg, offset);
}

Accessor* buildAccessor(uint32_t function, Process *p)
{
    if (!p)
    {
        return new ConstantAccessor(0, 0);
    }
    uint32_t method = function;
    uint32_t temp = function;
    int data_reg = -1;
    uint64_t v = p->readQuad(temp);

    Accessor* a = nullptr;

    if (do_match(temp, v, 2, 0xFFFF, 0xC033) ||
        do_match(temp, v, 2, 0xFFFF, 0xC031)) // XOR EAX, EAX
    {
        data_reg = 0;
        a = new ConstantAccessor(method, 0);
    }
    else if (do_match(temp, v, 3, 0xFFFFFF, 0xFFC883)) // OR EAX, -1
    {
        data_reg = 0;
        a = new ConstantAccessor(method, -1);
    }
    else if (do_match(temp, v, 5, 0xFF, 0xB8)) // MOV EAX,imm
    {
        data_reg = 0;a = new ConstantAccessor(method, (v>>8) & 0xFFFFFFFF);
    }
    else
    {
        DataWidth xsize;
        int ptr_reg = 1, tmp; // ECX

        // MOV REG,[ESP+4]
        if (do_match(temp, v, 4, 0xFFFFC7FFU, 0x0424448B))
        {
            ptr_reg = (v>>11)&7;
            v = p->readQuad(temp);
        }

        uint32_t offset;
        if (match_MOV_MEM(temp, v, ptr_reg, tmp, offset, xsize)) {
            data_reg = tmp;

            switch (xsize) {
            case Data32:
                v = p->readQuad(temp);

                uint32_t offset2;
                if (match_MOV_MEM(temp, v, data_reg, tmp, offset2, xsize)) {
                    data_reg = tmp;
                    switch (xsize) {
                    case Data32:
                            a = new IndirectAccessor<uint32_t>(method, p, offset, offset2);
                            break;
                    case DataSigned16:
                            a = new IndirectAccessor<int16_t>(method, p, offset, offset2);
                            break;
                    case DataUnsigned16:
                            a = new IndirectAccessor<uint16_t>(method, p, offset, offset2);
                            break;
                    default:
                            abort();
                    }
                }
                else {
                    a = new IndirectAccessor<uint32_t>(method, p, offset);
                }
            case DataSigned16:
                a = new IndirectAccessor<int16_t>(method, p, offset);
                break;
            case DataUnsigned16:
                a = new IndirectAccessor<uint16_t>(method, p, offset);
                break;
            default:
                abort();
            }
        }
    }

    v = p->readQuad(temp);

    if (data_reg == 0 && do_match(temp, v, 1, 0xFF, 0xC3)) // RET
        return a;
    else
    {
        delete a;
        printf("bad accessor @0x%x\n", function);
        return new ConstantAccessor(method, -1);
    }
}

// FIXME: turn into a proper factory with caching
Accessor * buildAccessor (OffsetGroup * I, Process * p, const char * name, uint32_t vtable)
{
    int32_t offset;
    if(I->getSafeOffset(name,offset))
    {
        return buildAccessor( p->readDWord( vtable + offset ), p);
    }
    else
    {
        fprintf(stderr,"Missing offset for item accessor \"%s\"\n", name);
        return new ConstantAccessor(-1,-1); // dummy accessor. always returns -1
    }
}

ItemDesc::ItemDesc(uint32_t VTable, Process *p)
{
    int32_t funcOffsetA, funcOffsetB, funcOffsetC, funcOffsetD, funcOffsetQuality, funcOffsetWear;
    OffsetGroup * Items = p->getDescriptor()->getGroup("Items");

    /* 
     * FIXME: and what about types, different sets of methods depending on class?
     * what about more complex things than constants and integers?
     * If this is to be generally useful, it needs much more power.
     */ 
    AMainType = buildAccessor(Items, p, "item_type_accessor", VTable);
    ASubType = buildAccessor(Items, p, "item_subtype_accessor", VTable);
    ASubIndex = buildAccessor(Items, p, "item_subindex_accessor", VTable);
    AIndex = buildAccessor(Items, p, "item_index_accessor", VTable);
    AQuality = buildAccessor(Items, p, "item_quality_accessor", VTable);
    AWear = buildAccessor(Items, p, "item_wear_accessor", VTable);

    this->vtable = VTable;
    this->p = p;
    this->className = p->readClassName(VTable).substr(5);
    this->className.resize(this->className.size()-2);

    this->hasDecoration = false;
    if(AMainType->isConstant())
        mainType = this->AMainType->getValue(0);
    else
    {
        fprintf(stderr, "Bad item main type at function %p\n", (void*) p->readDWord( VTable + funcOffsetA ));
        mainType = 0;
    }
}

string ItemDesc::dumpAccessors()
{
    std::stringstream outss;
    outss << "MainType  :" << AMainType->dump() << endl;
    outss << "ASubType  :" << ASubType->dump() << endl;
    outss << "ASubIndex :" << ASubIndex->dump() << endl;
    outss << "AIndex    :" << AIndex->dump() << endl;
    outss << "AQuality  :" << AQuality->dump() << endl;
    outss << "AWear     :" << AWear->dump() << endl;
    return outss.str();
}


bool ItemDesc::readItem(uint32_t itemptr, DFHack::dfh_item &item)
{
    p->read(itemptr, sizeof(t_item), (uint8_t*)&item.base);
    item.matdesc.itemType = AMainType->getValue(itemptr);
    item.matdesc.subType = ASubType->getValue(itemptr);
    item.matdesc.subIndex = ASubIndex->getValue(itemptr);
    item.matdesc.index = AIndex->getValue(itemptr);
    item.quality = AQuality->getValue(itemptr);
    item.quantity = 1; /* TODO */
    item.origin = itemptr;
    // FIXME: use templates. seriously.
    // Note: this accessor returns a 32-bit value with the higher
    // half sometimes containing garbage, so the cast is essential:
    item.wear_level = (int16_t)this->AWear->getValue(itemptr);
    return true;
}

class Items::Private
{
    public:
        DFContextShared *d;
        Process * owner;
        std::map<int32_t, ItemDesc *> descType;
        std::map<uint32_t, ItemDesc *> descVTable;
        uint32_t refVectorOffset;
        uint32_t refIDOffset;
        uint32_t ownerRefVTable;
};

Items::Items(DFContextShared * d_)
{
    d = new Private;
    d->d = d_;
    d->owner = d_->p;
    d->ownerRefVTable = d->refVectorOffset = d->refIDOffset = 0;
}

bool Items::Start()
{
    return true;
}

bool Items::Finish()
{
    return true;
}

Items::~Items()
{
    Finish();
    std::map<uint32_t, ItemDesc *>::iterator it;
    it = d->descVTable.begin();
    while (it != d->descVTable.end())
    {
        delete (*it).second;
        ++it;
    }
    d->descType.clear();
    d->descVTable.clear();
    delete d;
}

bool Items::readItem(uint32_t itemptr, DFHack::dfh_item &item)
{
    std::map<uint32_t, ItemDesc *>::iterator it;
    Process * p = d->owner;
    ItemDesc * desc;

    uint32_t vtable = p->readDWord(itemptr);
    it = d->descVTable.find(vtable);
    if(it == d->descVTable.end())
    {
        desc = new ItemDesc(vtable, p);
        d->descVTable[vtable] = desc;
        d->descType[desc->mainType] = desc;
    }
    else
        desc = it->second;

    return desc->readItem(itemptr, item);
}

bool Items::writeItem(const DFHack::dfh_item &item)
{
    if(item.origin)
    {
        d->owner->write(item.origin, sizeof(t_item),(uint8_t *)&(item.base));
        return true;
    }
    return false;
}

/*
void Items::setItemFlags(uint32_t itemptr, t_itemflags new_flags)
{
    d->owner->writeDWord(itemptr + 0x0C, new_flags.whole);
}
*/
int32_t Items::getItemOwnerID(const DFHack::dfh_item &item)
{
    if (!d->refVectorOffset)
    {
        OffsetGroup * Items = d->owner->getDescriptor()->getGroup("Items");
        d->refVectorOffset = Items->getOffset("item_ref_vector");
        d->refIDOffset = Items->getOffset("owner_ref_id_field");
    }

    DFHack::DfVector<uint32_t> p_refs(d->owner, item.origin + d->refVectorOffset);
    uint32_t size = p_refs.size();

    for (uint32_t i=0;i<size;i++)
    {
        uint32_t curRef = p_refs[i];
        uint32_t vtbl = d->owner->readDWord(curRef);

        if (!d->ownerRefVTable)
        {
            std::string className = d->owner->readClassName(vtbl);
            if (className == "general_ref_unit_itemownerst")
                d->ownerRefVTable = vtbl;
            else
                continue;
        }
        else if (d->ownerRefVTable != vtbl)
            continue;

        return d->owner->readDWord(curRef + d->refIDOffset);
    }

    return -1;
}

std::string Items::getItemClass(const dfh_item & item)
{
    return getItemClass(item.matdesc.itemType);
}

std::string Items::getItemClass(int32_t index)
{
    std::map<int32_t, ItemDesc *>::iterator it;
    std::string out;

    it = d->descType.find(index);
    if(it == d->descType.end())
    {
        /* these are dummy values for mood decoding */
        switch(index)
        {
            case 0: return "bar";
            case 1: return "cut gem";
            case 2: return "block";
            case 3: return "raw gem";
            case 4: return "raw stone";
            case 5: return "log";
            case 54: return "leather";
            case 57: return "cloth";
            case -1: return "probably bone or shell, but I really don't know";
            default: return "unknown";
        }
    }
    out = it->second->className;
    return out;
}

std::string Items::getItemDescription(const dfh_item & item, Materials * Materials)
{
    std::stringstream outss;
    switch(item.quality)
    {
        case 0:
            outss << "Ordinary ";
            break;
        case 1:
            outss << "Well crafted ";
            break;
        case 2:
            outss << "Finely crafted ";
            break;
        case 3:
            outss << "Superior quality ";
            break;
        case 4:
            outss << "Exceptionnal ";
            break;
        case 5:
            outss << "Masterful ";
            break;
        default: outss << "Crazy quality " << item.quality << " "; break;
    }
    outss << Materials->getDescription(item.matdesc) << " " << getItemClass(item.matdesc.itemType);
    return outss.str();
}

/// dump offsets used by accessors of a valid item to a string
std::string Items::dumpAccessors(const dfh_item & item)
{
    std::map< uint32_t, ItemDesc* >::const_iterator it = d->descVTable.find(item.base.vtable);
    if(it != d->descVTable.end())
        return it->second->dumpAccessors();
    return "crud";
}
