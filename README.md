# flatfs

* Small overhead ([extent based](https://en.wikipedia.org/wiki/Extent_\(file_systems\)), one extent per file)
* Write once (primarily, so we can have one extet per file).
* At least partial recoverability by linear sanning of underlying medium (files have headers with magic number, size, and checksum).
* Possible extensions that don't interfere with previous points:
    * Append to last created file (continue interrupted download)
    * Delete last created file (kinda pop it off the stack)

## SuperBlock

Block sizes are encoded as `(2^(7 + (min 5 (Word8))))`
```
  4B: FLAT
  2B: Version
      1B: Major (Word8)
      1B: Minor (Word8)
  1B: SuperBlockSize
      Block size, inode map of size: (X-64)/8-1
      0 -> 128B block, inode map size: 7
      1 -> 256B block, inode map size: 23
      2 -> 512B block, inode map size: 55
      3 -> 1024B block, inode map size: 119
      4 -> 2048B block, inode map size: 247
      5 -> 4096B block, inode map size: 247
  5B: zeros (alignmnt/reserved)
  1B: BlockSize (<=SuperBlockSize)
  8B: Size (Word64) Number of blocks (Not including SuperBlock)
  6B: "LABEL="
 16B: Label (zero padded)
  5B: "UUID="
 16B: UUID
[8B]: Extent map, list of file boundaries Word64
      list of blocks/positions of file header blocks
      (followed by files themselves)
  8B: Position of next extent map block (0 if none)
```


## File

```
  4B: "FILE"
 16B: MD5SUM
 24B: MD5SUM Context (for continuation)
255B: FILENAME (padded with 0s)
...
```

```
| Device                                        |
|---------------|---------------|---------------|
| Cluster       | Cluster       | Cluster       |
|               |
|               \_______________________________
|                                               \
|---------------|---------------|---------------|
| Block         | Block         | Block         |
```

We need:
* `mkfs.flat`: analogous to other mkfs things
* `flatfs`: fuse thing to access the filesystem
* `fsck.flat`: consistency check

# Playground

Prepare 1GB file to play with as partition...
```
dd if=/dev/zero of=disk.img bs=1M count=1024
```
