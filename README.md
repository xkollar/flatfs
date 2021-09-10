# flatfs

* Write once.
* Possibly:
    * Append to last created file
    * Delete last created file (kinda pop it off the stack)

## SuperBlock

```
  4B: FLAT
  2B: Version
      1B: Major (Word8)
      1B: Minor (Word8)
  8B: Size (Word64)
  1B: SuperBlockSize (2^(7 + (min 4 (Word8))))
      0 -> 8 inodes
      1 -> 24 inodes
      2 -> 56 inodes
      3 -> 120 inodes
      4 -> 248 inodes
    : "LABEL="
 16B: Label
    : "UUID="
 16B: UUID
  6B: zeros (alignmnt/reserved)
[8B]: List of cluster boundaries Word64
      list of clusters/blocks/positions of starts of files
  8B: Position of next inode block (0 if none)
```


## File

```
  4B: "FILE"
 16B: MD5SUM
  ?B: MD5SUM Context
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
