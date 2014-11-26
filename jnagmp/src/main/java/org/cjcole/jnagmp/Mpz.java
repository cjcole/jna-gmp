/*
 * Copyright 2014 Christopher Cole
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/*
 * GMP function documentation licensed under GNU Free Documentation License.
 * http://gmplib.org/manual/GNU-Free-Documentation-License.html
 *
 * Copyright 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
 * 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
 */
package org.cjcole.jnagmp;

import com.squareup.jnagmp.LibGmp;
import com.squareup.jnagmp.LibGmp.mpz_t;
import com.sun.jna.Memory;
import java.io.UnsupportedEncodingException;

public class Mpz {

  private static class MpzMemory extends Memory {
    public final mpz_t peer;

    MpzMemory() {
      super(mpz_t.SIZE);
      peer = new mpz_t(this);
      LibGmp.__gmpz_init(peer);
    }

    @Override protected void finalize() {
      LibGmp.__gmpz_clear(peer);
      super.finalize();
    }
  }

  private final MpzMemory mpzMemory = new MpzMemory();

  mpz_t getPeer() {
    return mpzMemory.peer;
  }

  public Mpz() {
  }

  public Mpz(Mpz op) {
    LibGmp.__gmpz_set(getPeer(), op.getPeer());
  }

  public Mpz(long op) {
    LibGmp.__gmpz_set_si(getPeer(), op);
  }

  public Mpz(String str, int base) throws UnsupportedEncodingException {
    byte[] bytes = str.getBytes("US-ASCII");
    Memory memory = new Memory(bytes.length + 1);
    memory.write(0, bytes, 0, bytes.length);
    memory.setByte(bytes.length, (byte) 0);
    LibGmp.__gmpz_set_str(getPeer(), memory, base);
  }

  public Mpz add(Mpz op) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_add(rop.getPeer(), getPeer(), op.getPeer());
    return rop;
  }

  public Mpz add(long op) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_add_ui(rop.getPeer(), getPeer(), op);
    return rop;
  }

  public Mpz sub(Mpz op) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_sub(rop.getPeer(), getPeer(), op.getPeer());
    return rop;
  }

  public Mpz sub(long op) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_sub_ui(rop.getPeer(), getPeer(), op);
    return rop;
  }

  public Mpz mul(Mpz op) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_mul(rop.getPeer(), getPeer(), op.getPeer());
    return rop;
  }

  public Mpz mul(long op) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_mul_ui(rop.getPeer(), getPeer(), op);
    return rop;
  }

  public Mpz cdivQ(Mpz d) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_cdiv_q(rop.getPeer(), getPeer(), d.getPeer());
    return rop;
  }

  public Mpz cdivQ(long d) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_cdiv_q_ui(rop.getPeer(), getPeer(), d);
    return rop;
  }

  public Mpz mod(Mpz op) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_mod(rop.getPeer(), getPeer(), op.getPeer());
    return rop;
  }

  public Mpz neg() {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_neg(rop.getPeer(), getPeer());
    return rop;
  }

  public Mpz powm(Mpz exp, Mpz mod) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_powm(rop.getPeer(), getPeer(), exp.getPeer(), mod.getPeer());
    return rop;
  }

  public Mpz invert(Mpz mod) {
    Mpz rop = new Mpz();
    LibGmp.__gmpz_invert(rop.getPeer(), getPeer(), mod.getPeer());
    return rop;
  }

  public long sizeinbase(int base) {
    return LibGmp.__gmpz_sizeinbase(getPeer(), base);
  }

  public int toInt() {
    return (int) LibGmp.__gmpz_get_ui(getPeer());
  }

  public int hashCode() {
    return toInt();
  }

  public boolean equals(Object object) {
    if (object instanceof Mpz) {
      return LibGmp.__gmpz_cmp(getPeer(), ((Mpz) object).getPeer()) == 0;
    } else if (object instanceof Long) {
      return LibGmp.__gmpz_cmp_si(getPeer(), ((Long) object).longValue()) == 0;
    } else {
      return false;
    }
  }

  public String toString(int base) {
    long size = sizeinbase(base);
    Memory str = new Memory(size + 2);
    LibGmp.__gmpz_get_str(str, base, getPeer());
    int len = (int) (str.size() - 1);
    assert (long) len == (str.size() - 1);
    byte[] bytes = new byte[len];
    str.read(0, bytes, 0, bytes.length);
    return new String(bytes);
  }

  public String toString() {
    return toString(10);
  }
}
