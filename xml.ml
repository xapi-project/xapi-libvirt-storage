(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let example_volume_xml = "
   <volume>
     <name>myvol</name>
     <key>rbd/myvol</key>
     <source>
     </source>
     <capacity unit='bytes'>53687091200</capacity>
     <allocation unit='bytes'>53687091200</allocation>
     <target>
       <path>rbd:rbd/myvol</path>
       <format type='unknown'/>
       <permissions>
         <mode>00</mode>
         <owner>0</owner>
         <group>0</group>
       </permissions>
     </target>
   </volume>
"

let example_pool_xml = "
<pool type='rbd'>
  <name>ceph</name>

  <source>
    <name>rbd</name>
    <host name='10.80.237.208' port='6789'/>
  </source>

</pool>
"

let read_xml_path path' xml =
  let input = Xmlm.make_input (`String (0, xml)) in
  let rec search path = match Xmlm.input input with
  | `Dtd _ -> search path
  | `El_start ((_, x), _) -> search (x :: path)
  | `El_end -> search (List.tl path)
  | `Data x when path = path' -> x
  | `Data _ -> search path in
  search []

let volume_target_path = [ "path"; "target"; "volume" ]

let name_pool = [ "name"; "pool" ]
