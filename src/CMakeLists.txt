#
# CMakeLists.txt libraries
#

#set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/src)

add_library(rimcommon STATIC
    parameters.f90
    attadd.f attdel.f attget.f attnew.f attpag.f attput.f
    blkchg.f blkcln.f blkclr.f blkdef.f blkdsp.f blkdwn.f blkext.f
    blkloc.f blkmov.f blknxt.f blkup.f
    btadd.f btget.f btinit.f btlki.f btlkr.f btmove.f btput.f btrep.f btsert.f
    change.f chkatt.f chkrel.f
    delete.f eq.f
    getkca.f gett.f gtsort.f
    isect.f
    join.f kmpard.f kmpari.f kmparr.f kmpart.f kompxx.f
    locatt.f locboo.f locprm.f locvar.f
    motscn.f ne.f 
    parse.f parval.f prckey.f prjtup.f
    putt.f ptrs.f
    rmlook.f rmpii.f rmres.f rmsav.f rmvarc.f rtoa.f
    selrel.f selrpt.f selsrt.f selwhr.f
    sort.f subtrc.f swcon.f swcost.f
    swfilo.f swflfs.f 
    swhart.f swhrtd.f swhrti.f swhrtr.f 
    swicst.f swidcp.f swiicp.f swinpo.f swircp.f switcp.f 
    swshel.f swsink.f swsmfl.f swsmvl.f 
    swunlo.f swunvl.f 
    swvacp.f swvlfs.f swvllo.f 
    union.f varadd.f 
    whetol.f wheval.f whrnam.f
    buffer_data.f90
    buffer.f90
    cards.f90
    datetime.f90
    extern.f90
    files.f90
    formater.f90
    globals.f90
    links_data.f90
    links.f90
    macros.f90
    maths.f90
    message.f90
    lexer.f90
    parser.f90
    randomfiles.f90
    relations_data.f90
    relations.f90
    rim.f90
    system.f90
    text.f90
    textfiles.f90
    utils.f90
)

add_library(rimextlib STATIC
    loadmv.f
    pgbres.f pgbsto.f pgcexp.f
    pgcomp.f pgcprt.f pgeexp.f pgeprt.f pgexec.f pgstor.f
    selatt.f selout.f selput.f90 sgtlnk.f 
    undata.f undef.f unpass.f
    extern_out.f90
    rim_cmd.f90
    system.f90
)

add_library(rimlib STATIC
    pilib.f rmxdel.f rmxget.f rmxlod.f rmxput.f
    extern_mute.f90
    system.f90
 )