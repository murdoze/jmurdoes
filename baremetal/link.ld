SECTIONS
{
    . = 0x7c00;
    .text :
    {
	FILL(0x90)

        __start = .;

        *(.text)

        . = 0x1FE;
        SHORT(0xAA55)

        *(.stage2)

        /*
        Number of sectors in stage 2. Used by the `int 13` to load it from disk.

        The value gets put into memory as the very last thing
        in the `.stage` section if it exists.

        We must put it *before* the final `. = ALIGN(512)`,
        or else it would fall out of the loaded memory.

        This must be absolute, or else it would get converted
        to the actual address relative to this section (7c00 + ...)
        and linking would fail with "Relocation truncated to fit"
        because we are trying to put that into al for the int 13.
         */
        __stage2_nsectors = ABSOLUTE((. - __start) / 512);

        /* Ensure that the generated image is a multiple of 512 bytes long. */
        . = ALIGN(512);
        __end = .;
        __end_align_4k = ALIGN(4k);
    }
}

