#!/usr/bin/env python3
"""
Measure actual deduplication rates in our consciousness substrate

Following Opus's advice: measure real things, not theoretical improvements.
"""

import os
import json
import hashlib
from pathlib import Path
from collections import defaultdict, Counter
import sys

def analyze_network_directory():
    """Analyze the existing ~/.vegur/network directory for deduplication opportunities"""
    
    vegur_dir = Path.home() / ".vegur"
    network_dir = vegur_dir / "network"
    
    if not network_dir.exists():
        print("❌ No ~/.vegur/network directory found")
        print("   Run some agents first to generate data")
        return
    
    print(f"📊 Analyzing consciousness network data in {network_dir}")
    print("=" * 60)
    
    # Collect all JSON files
    json_files = list(network_dir.glob("*.json"))
    total_files = len(json_files)
    
    if total_files == 0:
        print("❌ No JSON files found in network directory")
        return
    
    print(f"📁 Found {total_files} memory files")
    
    # Analyze file sizes
    total_size = sum(f.stat().st_size for f in json_files)
    print(f"💾 Total storage: {total_size:,} bytes ({total_size/1024:.1f} KB)")
    
    # Analyze content types
    content_types = Counter()
    content_hashes = defaultdict(list)  # hash -> list of files with that content
    knowledge_items = defaultdict(list)  # key -> list of values
    
    duplicate_content = 0
    total_content_size = 0
    
    for filepath in json_files:
        try:
            with open(filepath, 'r') as f:
                data = json.load(f)
            
            # Track content types
            if "memory_type" in data:
                content_types[data["memory_type"]] += 1
            elif "type" in data:
                content_types[data["type"]] += 1
            else:
                content_types["unknown"] += 1
            
            # Content-address the actual data (excluding metadata)
            content_data = {k: v for k, v in data.items() 
                          if k not in ["timestamp", "agent_id", "node_id"]}
            content_str = json.dumps(content_data, sort_keys=True)
            content_hash = hashlib.sha256(content_str.encode()).hexdigest()[:16]
            
            content_hashes[content_hash].append(filepath.name)
            total_content_size += len(content_str)
            
            # Track knowledge duplicates
            if data.get("type") == "knowledge":
                key = data.get("key", "unknown")
                value = data.get("value", "")
                knowledge_items[key].append(value)
            
        except (json.JSONDecodeError, OSError) as e:
            print(f"⚠️  Error reading {filepath.name}: {e}")
    
    # Calculate deduplication potential
    unique_contents = len(content_hashes)
    duplicate_contents = sum(len(files) - 1 for files in content_hashes.values() if len(files) > 1)
    
    print(f"\n🔍 Content Analysis:")
    print(f"   Unique contents: {unique_contents}")
    print(f"   Duplicate contents: {duplicate_contents}")
    
    if duplicate_contents > 0:
        dedup_savings = (duplicate_contents / total_files) * 100
        print(f"   💰 Potential storage savings: {dedup_savings:.1f}% ({duplicate_contents}/{total_files} files)")
    else:
        print(f"   ✨ No duplicate content found (good!)")
    
    print(f"\n📋 Content Types:")
    for content_type, count in content_types.most_common():
        percentage = (count / total_files) * 100
        print(f"   {content_type}: {count} files ({percentage:.1f}%)")
    
    # Show duplicates if any
    duplicates_found = 0
    for content_hash, files in content_hashes.items():
        if len(files) > 1:
            duplicates_found += 1
            print(f"\n🔄 Duplicate content group {duplicates_found}:")
            print(f"   Hash: {content_hash}")
            print(f"   Files: {', '.join(files)}")
    
    # Analyze knowledge redundancy
    knowledge_duplicates = {k: v for k, v in knowledge_items.items() if len(set(v)) < len(v)}
    if knowledge_duplicates:
        print(f"\n📚 Knowledge Redundancy:")
        for key, values in knowledge_duplicates.items():
            unique_values = set(values)
            redundancy = len(values) - len(unique_values)
            print(f"   '{key}': {len(values)} entries, {len(unique_values)} unique ({redundancy} redundant)")
    
    return {
        "total_files": total_files,
        "total_size": total_size,
        "unique_contents": unique_contents,
        "duplicate_contents": duplicate_contents,
        "potential_savings_percent": (duplicate_contents / total_files) * 100 if total_files > 0 else 0,
        "content_types": dict(content_types)
    }

def simulate_conversation_deduplication():
    """Simulate deduplication on common conversation patterns"""
    
    print(f"\n🎭 Simulating Conversation Deduplication")
    print("=" * 50)
    
    # Common conversation patterns that would appear in LLM interactions
    conversations = [
        "What is the capital of France?",
        "Paris is the capital of France.",
        "What is the capital of Germany?", 
        "Berlin is the capital of Germany.",
        "What is the capital of France?",  # Duplicate
        "Paris is the capital of France.",  # Duplicate
        "Tell me about machine learning.",
        "Machine learning is a subset of artificial intelligence.",
        "What is AI?",
        "AI stands for artificial intelligence.",
        "Tell me about machine learning.",  # Duplicate
        "Machine learning is a subset of artificial intelligence.",  # Duplicate
        "How does a neural network work?",
        "Neural networks process information through interconnected nodes.",
    ]
    
    print(f"📝 Testing with {len(conversations)} conversation snippets")
    
    # Calculate storage without deduplication
    total_size = sum(len(conv.encode()) for conv in conversations)
    
    # Calculate storage with content addressing
    unique_conversations = set(conversations)
    deduplicated_size = sum(len(conv.encode()) for conv in unique_conversations)
    
    savings = total_size - deduplicated_size
    savings_percent = (savings / total_size) * 100
    
    print(f"📊 Results:")
    print(f"   Total conversations: {len(conversations)}")
    print(f"   Unique conversations: {len(unique_conversations)}")
    print(f"   Storage without dedup: {total_size:,} bytes")
    print(f"   Storage with dedup: {deduplicated_size:,} bytes")
    print(f"   💰 Savings: {savings:,} bytes ({savings_percent:.1f}%)")
    
    # Show what gets deduplicated
    duplicate_count = Counter(conversations)
    duplicates = {conv: count for conv, count in duplicate_count.items() if count > 1}
    
    if duplicates:
        print(f"\n🔄 Duplicate conversations found:")
        for conv, count in duplicates.items():
            print(f"   '{conv[:50]}...': appears {count} times")
    
    return {
        "total_conversations": len(conversations),
        "unique_conversations": len(unique_conversations),
        "savings_bytes": savings,
        "savings_percent": savings_percent
    }

def main():
    """Run deduplication analysis"""
    
    print("🔬 Vegur Deduplication Analysis")
    print("Following Opus's guidance: measure real things!")
    print("=" * 60)
    
    # Analyze existing network data
    network_results = analyze_network_directory()
    
    # Simulate conversation deduplication
    conversation_results = simulate_conversation_deduplication()
    
    # Summary report
    print(f"\n📈 SUMMARY REPORT")
    print("=" * 30)
    
    if network_results:
        print(f"🗂️  Network Analysis:")
        print(f"   Files analyzed: {network_results['total_files']}")
        print(f"   Storage savings potential: {network_results['potential_savings_percent']:.1f}%")
    
    print(f"💬 Conversation Simulation:")
    print(f"   Deduplication savings: {conversation_results['savings_percent']:.1f}%")
    print(f"   Storage reduced by: {conversation_results['savings_bytes']:,} bytes")
    
    # Practical recommendations
    print(f"\n💡 PRACTICAL NEXT STEPS:")
    if network_results and network_results['potential_savings_percent'] > 5:
        print("   ✅ Network data shows significant deduplication potential")
        print("   → Implement content-addressed storage for network memories")
    else:
        print("   📝 Network data shows minimal duplication (system working well)")
        
    if conversation_results['savings_percent'] > 15:
        print("   ✅ Conversation deduplication could save significant storage")
        print("   → Implement conversation log deduplication")
    
    print(f"\n📋 Implementation Priority:")
    print("   1. Add SQLite storage with content addressing")
    print("   2. Measure deduplication on real agent conversations")
    print("   3. Add metrics dashboard for storage efficiency")
    
    return True

if __name__ == "__main__":
    main()